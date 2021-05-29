#![feature(raw_os_nonzero)]
#![deny(warnings)]

use educe::Educe;
use null_terminated::Nul;
use std::io::{self};
use std::mem::{MaybeUninit};
use std::os::raw::{NonZero_c_ushort, c_int};
use std::ptr::{null, null_mut, NonNull};
use winapi::shared::basetsd::LONG_PTR;
use winapi::shared::minwindef::{HINSTANCE__, UINT, WPARAM, LPARAM, LRESULT, LPVOID, TRUE};
use winapi::shared::windef::{HBRUSH, HWND, HWND__};
use winapi::um::libloaderapi::GetModuleHandleW;
use winapi::um::winuser::{LoadIconW, LoadCursorW, IDI_APPLICATION, IDC_ARROW, COLOR_WINDOW, RegisterClassW, WNDCLASSW, CS_HREDRAW, CS_VREDRAW, PostQuitMessage, DefWindowProcW, WM_DESTROY, WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, HWND_DESKTOP, CreateWindowExW, SW_SHOWDEFAULT, ShowWindow, GetMessageW, TranslateMessage, DispatchMessageW, UnregisterClassW, DestroyWindow, WM_NCCREATE, CREATESTRUCTW, SetWindowLongPtrW, GWLP_USERDATA, GetWindowLongPtrW};
use winapi::um::winnt::LPCWSTR;

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
pub struct WindowClass<'a> {
    atom: NonZero_ATOM,
    instance: &'a Instance,
}

impl<'a> WindowClass<'a> {
    pub fn as_atom(&self) -> NonZero_ATOM { self.atom }

    pub fn instance(&self) -> &'a Instance { self.instance }

    pub fn new(name: &Nul<u16>, instance: &'a Instance) -> io::Result<WindowClass<'a>> {
        let wnd_class = WNDCLASSW {
            style: CS_HREDRAW | CS_VREDRAW,
            cbClsExtra: 0,
            cbWndExtra: 0,
            hInstance: instance.as_handle().as_ptr(),
            hIcon: unsafe { LoadIconW(null_mut(), IDI_APPLICATION) },
            hCursor: unsafe { LoadCursorW(null_mut(), IDC_ARROW) },
            hbrBackground: (COLOR_WINDOW + 1) as HBRUSH,
            lpszMenuName: null(),
            lpszClassName: name.as_ptr(),
            lpfnWndProc: Some(wnd_proc)
        };
        let atom = NonZero_ATOM::new(unsafe { RegisterClassW(&wnd_class as *const _) }).ok_or_else(io::Error::last_os_error)?;
        Ok(WindowClass { atom, instance })
    }
}

impl<'a> Drop for WindowClass<'a> {
    fn drop(&mut self) {
        let ok = unsafe { UnregisterClassW(self.atom.get() as usize as LPCWSTR, self.instance.as_handle().as_ptr()) };
        assert_ne!(ok, 0, "UnregisterClassW failed");
    }
}

pub trait WindowImpl {
    fn destroy(&self, handled: &mut bool) { *handled = false; }
}

#[derive(Educe)]
#[educe(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Window<'a, 'b> {
    h_wnd: NonNull<HWND__>,
    #[educe(PartialOrd(ignore), Ord(ignore), PartialEq(ignore), Eq(ignore), Hash(ignore))]
    class: &'a WindowClass<'b>
}

impl<'a, 'b> Window<'a, 'b> {
    pub fn as_h_wnd(&self) -> NonNull<HWND__> { self.h_wnd }

    pub fn class(&self) -> &'a WindowClass<'b> { self.class }

    pub fn new(name: &Nul<u16>, class: &'a WindowClass<'b>, w_impl: Box<dyn WindowImpl>) -> io::Result<Window<'a, 'b>> {
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
    if message == WM_NCCREATE {
        let create_struct = l_param as *const CREATESTRUCTW;
        SetWindowLongPtrW(h_wnd, GWLP_USERDATA, (*create_struct).lpCreateParams as LONG_PTR);
        return TRUE as LRESULT;
    }
    let w_impl = &*(GetWindowLongPtrW(h_wnd, GWLP_USERDATA) as *mut Box<dyn WindowImpl>);
    match message {
        WM_DESTROY => {
            let mut handled = true;
            w_impl.destroy(&mut handled);
            if handled { return 0; }
        },
        _ => { },
    }
    DefWindowProcW(h_wnd, message, w_param, l_param)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
