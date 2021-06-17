#![feature(raw_os_nonzero)]
#![deny(warnings)]

use educe::Educe;
use enum_primitive_derive::Primitive;
use null_terminated::Nul;
use num_traits::ToPrimitive;
use std::fmt::{self, Display, Formatter};
use std::io::{self};
use std::marker::PhantomData;
use std::mem::{MaybeUninit, ManuallyDrop};
use std::ops::{Deref, DerefMut};
use std::os::raw::{NonZero_c_ushort, c_int};
use std::ptr::{null, null_mut, NonNull};
use winapi::shared::basetsd::LONG_PTR;
use winapi::shared::minwindef::{HINSTANCE__, HINSTANCE, UINT, WPARAM, LPARAM, LRESULT, LPVOID};
use winapi::shared::windef::{HBRUSH, HWND, HWND__, HDC__, HPEN__, HPEN, HGDIOBJ};
use winapi::um::libloaderapi::GetModuleHandleW;
use winapi::um::wingdi::{SetPixel, MoveToEx, LineTo, GetStockObject, WHITE_PEN, SelectObject, CreatePen};
use winapi::um::wingdi::{Rectangle, DeleteObject, PS_SOLID, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL, PS_INSIDEFRAME, PS_DASH};
use winapi::um::winnt::LPCWSTR;
use winapi::um::winuser::{LoadIconW, LoadCursorW, IDI_APPLICATION, IDC_ARROW, RegisterClassW, WNDCLASSW, CS_HREDRAW, CS_VREDRAW, PostQuitMessage, DefWindowProcW, WM_DESTROY, WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, HWND_DESKTOP, CreateWindowExW, SW_SHOWDEFAULT, ShowWindow, GetMessageW, TranslateMessage, DispatchMessageW, UnregisterClassW, DestroyWindow, WM_NCCREATE, CREATESTRUCTW, SetWindowLongPtrW, GWLP_USERDATA, GetWindowLongPtrW, WM_PAINT, BeginPaint, EndPaint, GWLP_HINSTANCE, GetClassWord, GCW_ATOM, GetClientRect, COLOR_3DFACE};

pub use winapi::shared::windef::COLORREF as COLORREF;
pub use winapi::shared::windef::POINT as POINT;
pub use winapi::shared::windef::RECT as RECT;
pub use winapi::um::wingdi::RGB as RGB;

#[doc(hidden)]
pub use utf16_lit::utf16_null as utf16_lit_utf16_null;
#[doc(hidden)]
pub use null_terminated::Nul as null_terminated_Nul;
use std::hint::unreachable_unchecked;

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct WStr<'a>(pub &'a Nul<u16>);

impl<'a> WStr<'a> {
    pub fn as_ptr(self) -> *const u16 { self.0.as_ptr() }
}

impl<'a> Display for WStr<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        String::from_utf16_lossy(&self.0[..]).fmt(f)
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Instance(NonNull<HINSTANCE__>);

impl Instance {
    pub fn as_handle(&self) -> NonNull<HINSTANCE__> { self.0 }

    pub fn current_executable() -> io::Result<Instance> {
        NonNull::new(unsafe { GetModuleHandleW(null()) }).ok_or_else(io::Error::last_os_error).map(Instance)
    }

    pub fn run() -> c_int {
        unsafe {
            let mut msg = MaybeUninit::uninit();
            while GetMessageW(msg.as_mut_ptr(), null_mut(), 0, 0) != 0 {
                TranslateMessage(msg.as_ptr());
                DispatchMessageW(msg.as_ptr());
            }
            msg.assume_init().wParam as _
        }
    }

    pub fn post_quit_message(exit_code: c_int) {
        unsafe { PostQuitMessage(exit_code) }
    }
}

#[allow(non_camel_case_types)]
pub type NonZero_ATOM = NonZero_c_ushort;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Class<'a> {
    atom: NonZero_ATOM,
    instance: &'a Instance,
}

impl<'a> Class<'a> {
    pub fn as_atom(&self) -> NonZero_ATOM { self.atom }

    pub fn instance(&self) -> &'a Instance { self.instance }

    pub fn new(name: WStr, instance: &'a Instance) -> io::Result<Class<'a>> {
        let wnd_class = WNDCLASSW {
            style: CS_HREDRAW | CS_VREDRAW,
            cbClsExtra: 0,
            cbWndExtra: 0,
            hInstance: instance.as_handle().as_ptr(),
            hIcon: unsafe { LoadIconW(null_mut(), IDI_APPLICATION) },
            hCursor: unsafe { LoadCursorW(null_mut(), IDC_ARROW) },
            hbrBackground: (COLOR_3DFACE + 1) as HBRUSH,
            lpszMenuName: null(),
            lpszClassName: name.as_ptr(),
            lpfnWndProc: Some(wnd_proc)
        };
        let atom = NonZero_ATOM::new(unsafe { RegisterClassW(&wnd_class as *const _) }).ok_or_else(io::Error::last_os_error)?;
        Ok(Class { atom, instance })
    }
}

impl<'a> Drop for Class<'a> {
    fn drop(&mut self) {
        let ok = unsafe { UnregisterClassW(self.atom.get() as usize as LPCWSTR, self.instance.as_handle().as_ptr()) };
        assert_ne!(ok, 0, "UnregisterClassW failed");
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct DeviceContext {
    h_dc: NonNull<HDC__>,
}


#[allow(non_camel_case_types)]
type HGDIOBJ__ = winapi::ctypes::c_void;

pub struct DeviceContextWithSelectedObject<'a, 'b> where 'a: 'b {
    context: &'a mut DeviceContext,
    object: PhantomData<&'b Pen>,
    original: NonNull<HGDIOBJ__>,
}

impl<'a, 'b> Deref for DeviceContextWithSelectedObject<'a, 'b> {
    type Target = DeviceContext;

    fn deref(&self) -> &DeviceContext { self.context }
}

impl<'a, 'b> DerefMut for DeviceContextWithSelectedObject<'a, 'b> {
    fn deref_mut(&mut self) -> &mut DeviceContext { self.context }
}

impl<'a, 'b> Drop for DeviceContextWithSelectedObject<'a, 'b> {
    fn drop(&mut self) {
        let object = unsafe { SelectObject(self.context.h_dc.as_ptr(), self.original.as_ptr()) };
        assert_ne!(object, null_mut(), "SelectObject failed");
    }
}

impl DeviceContext {
    pub fn set_pixel(&mut self, x: c_int, y: c_int, color: COLORREF) -> Option<COLORREF> {
        let res = unsafe { SetPixel(self.h_dc.as_ptr(), x, y, color) };
        if res == u32::MAX { None } else { Some(res) }
    }

    pub fn move_to_ex(&mut self, x: c_int, y: c_int) -> POINT {
        let mut res = MaybeUninit::uninit();
        let ok = unsafe { MoveToEx(self.h_dc.as_ptr(), x, y, res.as_mut_ptr()) };
        assert_ne!(ok, 0, "MoveToEx failed");
        unsafe { res.assume_init() }
    }

    pub fn line_to(&mut self, x: c_int, y: c_int) {
        let ok = unsafe { LineTo(self.h_dc.as_ptr(), x, y) };
        assert_ne!(ok, 0, "LineTo failed");
    }

    pub fn rectangle(&mut self, left: c_int, top: c_int, right: c_int, bottom: c_int) {
        let ok = unsafe { Rectangle(self.h_dc.as_ptr(), left, top, right, bottom) };
        assert_ne!(ok, 0, "Rectangle failed");
    }

    pub fn select_object<'a, 'b>(&'a mut self, object: &'b Pen) -> DeviceContextWithSelectedObject<'a, 'b> {
        let original = unsafe { SelectObject(self.h_dc.as_ptr(), object.0.as_ptr() as HGDIOBJ) };
        let original = NonNull::new(original).expect("SelectObject failed");
        DeviceContextWithSelectedObject { context: self, object: PhantomData, original }
    }
}


pub trait WindowImpl {
    fn destroy(&self, _window: &Window, handled: &mut bool) { *handled = false; }
    fn paint(&self, _window: &Window, _dc: &mut DeviceContext, _rect: RECT) { }
}

#[derive(Educe)]
#[educe(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Window<'a, 'b> {
    h_wnd: NonNull<HWND__>,
    #[educe(PartialOrd(ignore), Ord(ignore), PartialEq(ignore), Eq(ignore), Hash(ignore))]
    class: &'a Class<'b>
}

impl<'a, 'b> Window<'a, 'b> {
    pub fn as_h_wnd(&self) -> NonNull<HWND__> { self.h_wnd }

    pub fn class(&self) -> &'a Class<'b> { self.class }

    pub fn new(name: WStr, class: &'a Class<'b>, w_impl: Box<dyn WindowImpl>) -> io::Result<Window<'a, 'b>> {
        let h_wnd = unsafe { CreateWindowExW(
            0,
            class.as_atom().get() as usize as LPCWSTR,
            name.as_ptr(),
            WS_OVERLAPPEDWINDOW,
            CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
            HWND_DESKTOP,
            null_mut(),
            class.instance().as_handle().as_ptr(),
            Box::into_raw(Box::new(w_impl)) as LPVOID
        ) };
        NonNull::new(h_wnd).ok_or_else(io::Error::last_os_error).map(|h_wnd| Window { h_wnd, class })
    }

    pub fn show(&self) -> bool {
        unsafe { ShowWindow(self.h_wnd.as_ptr(), SW_SHOWDEFAULT) != 0 }
    }

    pub fn get_client_rect(&self) -> RECT {
        let mut rect = MaybeUninit::uninit();
        let ok = unsafe { GetClientRect(self.h_wnd.as_ptr(), rect.as_mut_ptr()) };
        if ok != 0 { Ok(()) } else { Err(io::Error::last_os_error()) }.expect("GetClientRect");
        unsafe { rect.assume_init() }
    }
}

impl<'a, 'b> Drop for Window<'a, 'b> {
    fn drop(&mut self) {
        let w_impl = unsafe { GetWindowLongPtrW(self.h_wnd.as_ptr(), GWLP_USERDATA) as *mut Box<dyn WindowImpl> };
        let ok = unsafe { DestroyWindow(self.h_wnd.as_ptr()) };
        assert_ne!(ok, 0, "DestroyWindow failed");
        unsafe { Box::from_raw(w_impl) };
    }
}

unsafe extern "system" fn wnd_proc(h_wnd: HWND, message: UINT, w_param: WPARAM, l_param: LPARAM) -> LRESULT {
    let h_wnd = NonNull::new_unchecked(h_wnd);
    if message == WM_NCCREATE {
        let create_struct = l_param as *const CREATESTRUCTW;
        SetWindowLongPtrW(h_wnd.as_ptr(), GWLP_USERDATA, (*create_struct).lpCreateParams as LONG_PTR);
    } else if let Some(w_impl) = NonNull::new(GetWindowLongPtrW(h_wnd.as_ptr(), GWLP_USERDATA) as *mut Box<dyn WindowImpl>) {
        let w_impl = w_impl.as_ref();
        let instance = ManuallyDrop::new(Instance(NonNull::new(GetWindowLongPtrW(h_wnd.as_ptr(), GWLP_HINSTANCE) as HINSTANCE)
            .ok_or_else(io::Error::last_os_error).expect("GetWindowLongPtrW|GWLP_HINSTANCE")));
        let class = ManuallyDrop::new(Class {
            instance: &instance,
            atom: NonZero_ATOM::new(GetClassWord(h_wnd.as_ptr(), GCW_ATOM))
                .ok_or_else(io::Error::last_os_error).expect("GetClassWord|GCW_ATOM")
        });
        let window = ManuallyDrop::new(Window { class: &class, h_wnd });
        match message {
            WM_DESTROY => {
                let mut handled = true;
                w_impl.destroy(&window, &mut handled);
                if handled { return 0; }
            },
            WM_PAINT => {
                let mut ps = MaybeUninit::uninit();
                let ok = BeginPaint(h_wnd.as_ptr(), ps.as_mut_ptr());
                assert_ne!(ok, null_mut(), "BeginPaint failed");
                let ps = ps.assume_init();
                w_impl.paint(&window, &mut DeviceContext { h_dc: NonNull::new_unchecked(ps.hdc) }, ps.rcPaint);
                EndPaint(h_wnd.as_ptr(), &ps as *const _);
                return 0;
            },
            _ => {},
        }
    }
    DefWindowProcW(h_wnd.as_ptr(), message, w_param, l_param)
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Pen(NonNull<HPEN__>);

impl Drop for Pen {
    fn drop(&mut self) {
        let ok = unsafe { DeleteObject(self.0.as_ptr() as HGDIOBJ) };
        assert_ne!(ok, 0, "DeleteObject failed");
    }
}

#[derive(Primitive)]
#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
#[derive(Debug)]
#[repr(i32)]
pub enum PenStyle {
    Solid = PS_SOLID as i32,
    Dash = PS_DASH as i32,
    Dot = PS_DOT as i32,
    DashDot = PS_DASHDOT as i32,
    DashDotDot = PS_DASHDOTDOT as i32,
    Null = PS_NULL as i32,
    InsideFrame = PS_INSIDEFRAME as i32,
}

impl Pen {
    pub fn new(style: PenStyle, width: c_int, color: COLORREF) -> Pen {
        let pen = unsafe { NonNull::new(CreatePen(style.to_i32().unwrap_or_else(|| unreachable_unchecked()), width, color)) };
        Pen(pen.expect("CreatePen failed"))
    }
}

pub enum Stock { }

impl Stock {
    pub fn white_pen() -> Pen {
        Pen(NonNull::new(unsafe { GetStockObject(WHITE_PEN as c_int) as HPEN }).expect("GetStockObject|WHITE_PEN failed"))
    }
}

#[macro_export]
macro_rules! w_str {
    ($($tokens:tt)*) => {
        $crate::WStr(unsafe {
            $crate::null_terminated_Nul::new_unchecked(
                &$crate::utf16_lit_utf16_null!($($tokens)*) as *const _
            )
        })
    };
}