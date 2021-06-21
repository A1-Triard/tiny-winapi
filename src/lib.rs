#![feature(raw_os_nonzero)]
#![feature(const_fn_transmute)]
#![deny(warnings)]

pub use winapi::shared::windef::COLORREF as COLORREF;
pub use winapi::shared::windef::POINT as POINT;
pub use winapi::shared::windef::RECT as RECT;
pub use winapi::um::wingdi::RGB as RGB;
pub use winapi::um::wingdi::TEXTMETRICW as TEXTMETRICW;

#[doc(hidden)]
pub use utf16_lit::utf16 as utf16_lit_utf16;
#[doc(hidden)]
pub use utf16_lit::utf16_null as utf16_lit_utf16_null;
#[doc(hidden)]
pub use null_terminated::Nul as null_terminated_Nul;

use educe::Educe;
use enum_primitive_derive::Primitive;
use null_terminated::Nul;
use num_traits::ToPrimitive;
use sealed::sealed;
use std::convert::TryInto;
use std::fmt::{self, Display, Formatter};
use std::hint::unreachable_unchecked;
use std::io::{self};
use std::marker::PhantomData;
use std::mem::{MaybeUninit, ManuallyDrop, transmute, size_of};
use std::ops::{Deref, DerefMut};
use std::os::raw::{NonZero_c_ushort, c_int};
use std::ptr::{null, null_mut, NonNull};
use winapi::shared::basetsd::LONG_PTR;
use winapi::shared::minwindef::{HINSTANCE__, HINSTANCE, UINT, WPARAM, LPARAM, LRESULT, LPVOID};
use winapi::shared::windef::{HBRUSH, HWND, HWND__, HDC__, HPEN__, HPEN, HGDIOBJ, HBRUSH__};
use winapi::um::libloaderapi::GetModuleHandleW;
use winapi::um::playsoundapi::{PlaySoundW, SND_FILENAME, SND_ASYNC};
use winapi::um::wingdi::*;
use winapi::um::winnt::LPCWSTR;
use winapi::um::winuser::*;
use std::iter::FromIterator;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct WStrZ(Nul<u16>);

impl WStrZ {
    pub const fn from(nul: &Nul<u16>) -> &WStrZ { unsafe { transmute(nul) } }

    pub const fn as_ptr(&self) -> *const u16 { self.0.as_ptr() }

    pub fn len(&self) -> usize { self.0.len() }

    pub fn as_w_str(&self) -> &WStr { (&self.0[..]).into() }
}

impl<'a> From<&'a Nul<u16>> for &'a WStrZ {
    fn from(nul: &'a Nul<u16>) -> &'a WStrZ { unsafe { transmute(nul) } }
}

impl AsRef<WStrZ> for WStrZ {
    fn as_ref(&self) -> &WStrZ { self }
}

impl AsRef<Nul<u16>> for WStrZ {
    fn as_ref(&self) -> &Nul<u16> { &self.0 }
}

impl Display for WStrZ {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        String::from_utf16_lossy(&self.0[..]).fmt(f)
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct WStr([u16]);

impl WStr {
    pub const fn from(slice: &[u16]) -> &WStr { unsafe { transmute(slice) } }

    pub const fn as_ptr(&self) -> *const u16 { self.0.as_ptr() }

    pub const fn len(&self) -> usize { self.0.len() }
}

impl<'a> From<&'a [u16]> for &'a WStr {
    fn from(slice: &'a [u16]) -> &'a WStr { unsafe { transmute(slice) } }
}

impl AsRef<WStr> for WStr {
    fn as_ref(&self) -> &WStr { self }
}

impl AsRef<[u16]> for WStr {
    fn as_ref(&self) -> &[u16] { &self.0 }
}

impl Display for WStr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        String::from_utf16_lossy(&self.0).fmt(f)
    }
}

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct WString(Vec<u16>);

impl<'a> From<&'a WStr> for WString {
    fn from(str: &'a WStr) -> WString { WString(Vec::from(str.as_ref())) }
}

impl From<Vec<u16>> for WString {
    fn from(vec: Vec<u16>) -> WString { WString(vec) }
}

impl Into<Vec<u16>> for WString {
    fn into(self) -> Vec<u16> { self.0 }
}

impl AsRef<WStr> for WString {
    fn as_ref(&self) -> &WStr { (&self.0[..]).into() }
}

impl Display for WString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s: &WStr = self.as_ref();
        s.fmt(f)
    }
}

impl FromIterator<u16> for WString {
    fn from_iter<I: IntoIterator<Item=u16>>(iter: I) -> WString { iter.into_iter().collect::<Vec<u16>>().into() }
}

impl<'a> FromIterator<&'a u16> for WString {
    fn from_iter<I: IntoIterator<Item=&'a u16>>(iter: I) -> WString { iter.into_iter().copied().collect::<Vec<u16>>().into() }
}

#[derive(Primitive)]
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
#[repr(i32)]
pub enum SystemMetrics {
    CXScreen = SM_CXSCREEN,
    CYScreen = SM_CYSCREEN,
    CXVScroll = SM_CXVSCROLL,
    CYHScroll = SM_CYHSCROLL,
    CYCaption = SM_CYCAPTION,
    CXBorder = SM_CXBORDER,
    CYBorder = SM_CYBORDER,
    CXDlgFrame = SM_CXDLGFRAME,
    CYDlgFrame = SM_CYDLGFRAME,
    CYVThumb = SM_CYVTHUMB,
    CXHThumb = SM_CXHTHUMB,
    CXIcon = SM_CXICON,
    CYIcon = SM_CYICON,
    CXCursor = SM_CXCURSOR,
    CYCursor = SM_CYCURSOR,
    CYMenu = SM_CYMENU,
    CXFullscreen = SM_CXFULLSCREEN,
    CYFullscreen = SM_CYFULLSCREEN,
    CYKanjiWindow = SM_CYKANJIWINDOW,
    MousePresent = SM_MOUSEPRESENT,
    CYVScroll = SM_CYVSCROLL,
    CXHScroll = SM_CXHSCROLL,
    Debug = SM_DEBUG,
    SwapButton = SM_SWAPBUTTON,
    Reserved1 = SM_RESERVED1,
    Reserved2 = SM_RESERVED2,
    Reserved3 = SM_RESERVED3,
    Reserved4 = SM_RESERVED4,
    CXMin = SM_CXMIN,
    CYMin = SM_CYMIN,
    CXSize = SM_CXSIZE,
    CYSize = SM_CYSIZE,
    CXFrame = SM_CXFRAME,
    CYFrame = SM_CYFRAME,
    CXMinTrack = SM_CXMINTRACK,
    CYMinTrack = SM_CYMINTRACK,
    CXDoubleClk = SM_CXDOUBLECLK,
    CYDoubleClk = SM_CYDOUBLECLK,
    CXIconSpacing = SM_CXICONSPACING,
    CYIconSpacing = SM_CYICONSPACING,
    MenuDropAlignment = SM_MENUDROPALIGNMENT,
    PenWindows = SM_PENWINDOWS,
    DbcsEnabled = SM_DBCSENABLED,
    CMouseButtons = SM_CMOUSEBUTTONS,
    ShowSounds = SM_SHOWSOUNDS,
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

    pub fn play_sound(file: &WStrZ) {
        let ok = unsafe { PlaySoundW(file.as_ptr(), null_mut(), SND_FILENAME | SND_ASYNC ) };
        assert_ne!(ok, 0, "PlaySoundW failed")
    }

    pub fn get_system_metrics(index: SystemMetrics) -> c_int {
        unsafe { GetSystemMetrics(index.to_i32().unwrap_or_else(|| unreachable_unchecked() )) }
    }
}

#[derive(Primitive)]
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
#[repr(u32)]
pub enum SystemColor {
    ActiveBorder = COLOR_ACTIVEBORDER as u32,
    ActiveCaption = COLOR_ACTIVECAPTION as u32,
    AppWorkspace = COLOR_APPWORKSPACE as u32,
    Background = COLOR_BACKGROUND as u32,
    BtnFace = COLOR_BTNFACE as u32,
    BtnShadow = COLOR_BTNSHADOW as u32,
    BtnText = COLOR_BTNTEXT as u32,
    CaptionText = COLOR_CAPTIONTEXT as u32,
    GrayText = COLOR_GRAYTEXT as u32,
    Highlight = COLOR_HIGHLIGHT as u32,
    HighlightText = COLOR_HIGHLIGHTTEXT as u32,
    InactiveBorder = COLOR_INACTIVEBORDER as u32,
    InactiveCaption = COLOR_INACTIVECAPTION as u32,
    Menu = COLOR_MENU as u32,
    MenuText = COLOR_MENUTEXT as u32,
    ScrollBar = COLOR_SCROLLBAR as u32,
    Window = COLOR_WINDOW as u32,
    WindowFrame = COLOR_WINDOWFRAME as u32,
    WindowText = COLOR_WINDOWTEXT as u32,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
pub enum WindowBackground<'a> {
    None,
    System(SystemColor),
    Brush(&'a Brush)
}

#[allow(non_camel_case_types)]
pub type NonZero_ATOM = NonZero_c_ushort;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Class<'i> {
    atom: NonZero_ATOM,
    instance: &'i Instance,
}

impl<'i> Class<'i> {
    pub fn as_atom(&self) -> NonZero_ATOM { self.atom }

    pub fn instance(&self) -> &'i Instance { self.instance }

    pub fn new(name: impl AsRef<WStrZ>, instance: &'i Instance, background: WindowBackground) -> io::Result<Class<'i>> {
        let background = match background {
            WindowBackground::None => null_mut(),
            WindowBackground::System(color) => (color.to_u32().unwrap_or_else(|| unsafe { unreachable_unchecked() }) + 1) as HBRUSH,
            WindowBackground::Brush(brush) => brush.0.as_ptr()
        };
        let wnd_class = WNDCLASSEXW {
            cbSize: size_of::<WNDCLASSEXW>() as _,
            style: CS_HREDRAW | CS_VREDRAW,
            cbClsExtra: 0,
            cbWndExtra: 0,
            hInstance: instance.as_handle().as_ptr(),
            hIcon: unsafe { LoadIconW(null_mut(), IDI_APPLICATION) },
            hIconSm: unsafe { LoadIconW(null_mut(), IDI_APPLICATION) },
            hCursor: unsafe { LoadCursorW(null_mut(), IDC_ARROW) },
            hbrBackground: background,
            lpszMenuName: null(),
            lpszClassName: name.as_ref().as_ptr(),
            lpfnWndProc: Some(wnd_proc)
        };
        let atom = NonZero_ATOM::new(unsafe { RegisterClassExW(&wnd_class as *const _) }).ok_or_else(io::Error::last_os_error)?;
        Ok(Class { atom, instance })
    }
}

impl<'i> Drop for Class<'i> {
    fn drop(&mut self) {
        let ok = unsafe { UnregisterClassW(self.atom.get() as usize as LPCWSTR, self.instance.as_handle().as_ptr()) };
        assert_ne!(ok, 0, "UnregisterClassW failed");
    }
}

#[derive(Primitive)]
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
#[repr(u32)]
pub enum TextVertAlign {
    Baseline = TA_BASELINE,
    Bottom = TA_BOTTOM,
    Top = TA_TOP,
}

#[derive(Primitive)]
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone)]
#[repr(u32)]
pub enum TextHorzAlign {
    Center = TA_CENTER,
    Left = TA_LEFT,
    Right = TA_RIGHT,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct DeviceContext(NonNull<HDC__>);

#[allow(non_camel_case_types)]
type HGDIOBJ__ = winapi::ctypes::c_void;

#[sealed]
pub trait GdiObject {
    fn h_gdi_obj(&self) -> NonNull<HGDIOBJ__>;
}

pub struct DeviceContextWithSelectedObject<'d, 'o> where 'd: 'o {
    context: &'d mut DeviceContext,
    object: PhantomData<&'o dyn GdiObject>,
    original: NonNull<HGDIOBJ__>,
}

impl<'d, 'o> Deref for DeviceContextWithSelectedObject<'d, 'o> {
    type Target = DeviceContext;

    fn deref(&self) -> &DeviceContext { self.context }
}

impl<'d, 'o> DerefMut for DeviceContextWithSelectedObject<'d, 'o> {
    fn deref_mut(&mut self) -> &mut DeviceContext { self.context }
}

impl<'d, 'o> Drop for DeviceContextWithSelectedObject<'d, 'o> {
    fn drop(&mut self) {
        let object = unsafe { SelectObject(self.context.0.as_ptr(), self.original.as_ptr()) };
        assert_ne!(object, null_mut(), "SelectObject failed");
    }
}

impl DeviceContext {
    pub fn set_pixel(&mut self, x: c_int, y: c_int, color: COLORREF) -> Option<COLORREF> {
        let res = unsafe { SetPixel(self.0.as_ptr(), x, y, color) };
        if res == u32::MAX { None } else { Some(res) }
    }

    pub fn move_to_ex(&mut self, x: c_int, y: c_int) -> POINT {
        let mut res = MaybeUninit::uninit();
        let ok = unsafe { MoveToEx(self.0.as_ptr(), x, y, res.as_mut_ptr()) };
        assert_ne!(ok, 0, "MoveToEx failed");
        unsafe { res.assume_init() }
    }

    pub fn line_to(&mut self, x: c_int, y: c_int) {
        let ok = unsafe { LineTo(self.0.as_ptr(), x, y) };
        assert_ne!(ok, 0, "LineTo failed");
    }

    pub fn rectangle(&mut self, left: c_int, top: c_int, right: c_int, bottom: c_int) {
        let ok = unsafe { Rectangle(self.0.as_ptr(), left, top, right, bottom) };
        assert_ne!(ok, 0, "Rectangle failed");
    }

    pub fn select_object<'a, 'b>(&'a mut self, object: &'b dyn GdiObject) -> DeviceContextWithSelectedObject<'a, 'b> {
        let original = unsafe { SelectObject(self.0.as_ptr(), object.h_gdi_obj().as_ptr() as HGDIOBJ) };
        let original = NonNull::new(original).expect("SelectObject failed");
        DeviceContextWithSelectedObject { context: self, object: PhantomData, original }
    }

    pub fn draw_text(&mut self, text: impl AsRef<WStr>, rect: &RECT) {
        let text = text.as_ref();
        let ok = unsafe { DrawTextW(self.0.as_ptr(), text.as_ptr(), text.len().try_into().expect("Too long text"), rect as *const _ as *mut _, DT_SINGLELINE | DT_CENTER | DT_VCENTER) };
        assert_ne!(ok, 0, "DrawTextW failed");
    }

    pub fn text_out(&mut self, x: c_int, y: c_int, text: impl AsRef<WStr>) {
        let text = text.as_ref();
        let ok = unsafe { TextOutW(self.0.as_ptr(), x, y, text.as_ptr(), text.len().try_into().expect("Too long text")) };
        assert_ne!(ok, 0, "TextOutW failed");
    }

    pub fn get_text_metrics(&self) -> TEXTMETRICW {
        let mut tm = MaybeUninit::uninit();
        let ok = unsafe { GetTextMetricsW(self.0.as_ptr(), tm.as_mut_ptr()) };
        assert_ne!(ok, 0, "GetTextMetricsW failed");
        unsafe { tm.assume_init() }
    }

    pub fn set_text_align(&mut self, horz: TextHorzAlign, vert: TextVertAlign) {
        let horz = horz.to_u32().unwrap_or_else(|| unsafe { unreachable_unchecked() });
        let vert = vert.to_u32().unwrap_or_else(|| unsafe { unreachable_unchecked() });
        let ok = unsafe { SetTextAlign(self.0.as_ptr(), horz | vert) };
        assert_ne!(ok, GDI_ERROR, "SetTextAlign failed");
    }
}

pub type GetDC = dyn for<'w, 'c, 'i> Fn(&'w Window<'c, 'i>) -> WindowDeviceContext<'w, 'c, 'i>;

pub trait WindowProc {
    fn create(&self, _window: &Window, _get_dc: &GetDC) { }
    fn destroy(&self, _window: &Window, handled: &mut bool) { *handled = false; }
    fn paint(&self, _window: &Window, _dc: &mut DeviceContext, _rect: RECT) { }
}

impl WindowProc for () { }

#[derive(Educe)]
#[educe(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Window<'c, 'i> {
    h_wnd: NonNull<HWND__>,
    #[educe(PartialOrd(ignore), Ord(ignore), PartialEq(ignore), Eq(ignore), Hash(ignore))]
    class: &'c Class<'i>
}

impl<'c, 'i> Window<'c, 'i> {
    pub fn as_h_wnd(&self) -> NonNull<HWND__> { self.h_wnd }

    pub fn class(&self) -> &'c Class<'i> { self.class }

    pub fn new(name: impl AsRef<WStrZ>, class: &'c Class<'i>, w_proc: Box<dyn WindowProc>) -> io::Result<Window<'c, 'i>> {
        let h_wnd = unsafe { CreateWindowExW(
            0,
            class.as_atom().get() as usize as LPCWSTR,
            name.as_ref().as_ptr(),
            WS_OVERLAPPEDWINDOW,
            CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
            HWND_DESKTOP,
            null_mut(),
            class.instance().as_handle().as_ptr(),
            Box::into_raw(Box::new(w_proc)) as LPVOID
        ) };
        NonNull::new(h_wnd).ok_or_else(io::Error::last_os_error).map(|h_wnd| Window { h_wnd, class })
    }

    pub fn show(&self) -> bool {
        unsafe { ShowWindow(self.h_wnd.as_ptr(), SW_SHOWDEFAULT) != 0 }
    }

    pub fn update(&self) {
        let ok = unsafe { UpdateWindow(self.h_wnd.as_ptr()) };
        assert_ne!(ok, 0, "UpdateWindow failed");
    }

    pub fn get_client_rect(&self) -> RECT {
        let mut rect = MaybeUninit::uninit();
        let ok = unsafe { GetClientRect(self.h_wnd.as_ptr(), rect.as_mut_ptr()) };
        if ok != 0 { Ok(()) } else { Err(io::Error::last_os_error()) }.expect("GetClientRect");
        unsafe { rect.assume_init() }
    }
}

impl<'c, 'i> Drop for Window<'c, 'i> {
    fn drop(&mut self) {
        let w_proc = unsafe { GetWindowLongPtrW(self.h_wnd.as_ptr(), GWLP_USERDATA) as *mut Box<dyn WindowProc> };
        let ok = unsafe { DestroyWindow(self.h_wnd.as_ptr()) };
        assert_ne!(ok, 0, "DestroyWindow failed");
        unsafe { Box::from_raw(w_proc) };
    }
}

pub struct WindowDeviceContext<'w, 'c, 'i> {
    context: DeviceContext,
    window: &'w Window<'c, 'i>
}

impl<'w, 'c, 'i> Deref for WindowDeviceContext<'w, 'c, 'i> {
    type Target = DeviceContext;

    fn deref(&self) -> &DeviceContext { &self.context }
}

impl<'w, 'c, 'i> DerefMut for WindowDeviceContext<'w, 'c, 'i> {
    fn deref_mut(&mut self) -> &mut DeviceContext { &mut self.context }
}

impl<'w, 'c, 'i> Drop for WindowDeviceContext<'w, 'c, 'i> {
    fn drop(&mut self) {
        let ok = unsafe { ReleaseDC(self.window.h_wnd.as_ptr(), self.context.0.as_ptr()) };
        assert_ne!(ok, 0, "ReleaseDC failed");
    }
}

fn get_dc<'w, 'c, 'i>(window: &'w Window<'c, 'i>) -> WindowDeviceContext<'w, 'c, 'i> {
    let context = NonNull::new(unsafe { GetDC(window.h_wnd.as_ptr()) }).expect("GetDC failed");
    WindowDeviceContext { window, context: DeviceContext(context) }
}

unsafe extern "system" fn wnd_proc(h_wnd: HWND, message: UINT, w_param: WPARAM, l_param: LPARAM) -> LRESULT {
    let h_wnd = NonNull::new_unchecked(h_wnd);
    if message == WM_NCCREATE {
        let create_struct = l_param as *const CREATESTRUCTW;
        SetWindowLongPtrW(h_wnd.as_ptr(), GWLP_USERDATA, (*create_struct).lpCreateParams as LONG_PTR);
    } else if let Some(w_proc) = NonNull::new(GetWindowLongPtrW(h_wnd.as_ptr(), GWLP_USERDATA) as *mut Box<dyn WindowProc>) {
        let w_proc = w_proc.as_ref();
        let instance = ManuallyDrop::new(Instance(NonNull::new(GetWindowLongPtrW(h_wnd.as_ptr(), GWLP_HINSTANCE) as HINSTANCE)
            .ok_or_else(io::Error::last_os_error).expect("GetWindowLongPtrW|GWLP_HINSTANCE")));
        let class = ManuallyDrop::new(Class {
            instance: &instance,
            atom: NonZero_ATOM::new(GetClassWord(h_wnd.as_ptr(), GCW_ATOM))
                .ok_or_else(io::Error::last_os_error).expect("GetClassWord|GCW_ATOM")
        });
        let window = ManuallyDrop::new(Window { class: &class, h_wnd });
        match message {
            WM_CREATE => {
                w_proc.create(&window, &get_dc);
                return 0;
            }
            WM_DESTROY => {
                let mut handled = true;
                w_proc.destroy(&window, &mut handled);
                if handled { return 0; }
            },
            WM_PAINT => {
                let mut ps = MaybeUninit::uninit();
                let ok = BeginPaint(h_wnd.as_ptr(), ps.as_mut_ptr());
                assert_ne!(ok, null_mut(), "BeginPaint failed");
                let ps = ps.assume_init();
                w_proc.paint(&window, &mut DeviceContext(NonNull::new_unchecked(ps.hdc)), ps.rcPaint);
                EndPaint(h_wnd.as_ptr(), &ps as *const _);
                return 0;
            },
            _ => { },
        }
    }
    DefWindowProcW(h_wnd.as_ptr(), message, w_param, l_param)
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Brush(NonNull<HBRUSH__>);

impl Drop for Brush {
    fn drop(&mut self) {
        let ok = unsafe { DeleteObject(self.0.as_ptr() as HGDIOBJ) };
        assert_ne!(ok, 0, "DeleteObject failed");
    }
}

#[sealed]
impl GdiObject for Brush {
    fn h_gdi_obj(&self) -> NonNull<HGDIOBJ__> { unsafe { transmute(self.0) } }
}

impl Brush {
    pub fn new_solid(color: COLORREF) -> Brush {
        let brush = unsafe { NonNull::new(CreateSolidBrush(color)) };
        Brush(brush.expect("CreateSolidBrush failed"))
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Pen(NonNull<HPEN__>);

impl Drop for Pen {
    fn drop(&mut self) {
        let ok = unsafe { DeleteObject(self.0.as_ptr() as HGDIOBJ) };
        assert_ne!(ok, 0, "DeleteObject failed");
    }
}

#[sealed]
impl GdiObject for Pen {
    fn h_gdi_obj(&self) -> NonNull<HGDIOBJ__> { unsafe { transmute(self.0) } }
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

    pub fn white_brush() -> Brush {
        Brush(NonNull::new(unsafe { GetStockObject(WHITE_BRUSH as c_int) as HBRUSH }).expect("GetStockObject|WHITE_BRUSH failed"))
    }
}

#[macro_export]
macro_rules! w_str_z {
    ($($tokens:tt)*) => {
        $crate::WStrZ::from(unsafe {
            $crate::null_terminated_Nul::new_unchecked(
                &$crate::utf16_lit_utf16_null!($($tokens)*) as *const _
            )
        })
    };
}

#[macro_export]
macro_rules! w_str {
    ($($tokens:tt)*) => {
        $crate::WStr::from(
            &$crate::utf16_lit_utf16!($($tokens)*)
        )
    };
}
