#![feature(raw_os_nonzero)]
#![deny(warnings)]

use educe::Educe;
use null_terminated::Nul;
use phantom_type::PhantomType;
use std::io::{self};
use std::mem::{MaybeUninit, forget};
use std::os::raw::{NonZero_c_ushort, c_int};
use std::ptr::{null, null_mut, NonNull};
use winapi::shared::minwindef::{HINSTANCE__, UINT, WPARAM, LPARAM, LRESULT, LPVOID};
use winapi::shared::windef::{HBRUSH, HWND, HWND__};
use winapi::um::libloaderapi::GetModuleHandleW;
use winapi::um::winuser::{LoadIconW, LoadCursorW, IDI_APPLICATION, IDC_ARROW, COLOR_WINDOW, RegisterClassW, WNDCLASSW, CS_HREDRAW, CS_VREDRAW, PostQuitMessage, DefWindowProcW, WM_DESTROY, WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, HWND_DESKTOP, CreateWindowExW, SW_SHOWDEFAULT, ShowWindow, GetMessageW, TranslateMessage, DispatchMessageW, UnregisterClassW};
use winapi::um::winnt::LPCWSTR;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Instance(NonNull<HINSTANCE__>);

impl Instance {
    pub unsafe fn from_raw(h_instance: NonNull<HINSTANCE__>) -> Instance {
        Instance(h_instance)
    }

    pub fn into_raw(self) -> NonNull<HINSTANCE__> {
        let raw = self.0;
        forget(self);
        raw
    }

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
}

#[allow(non_camel_case_types)]
pub type NonZero_ATOM = NonZero_c_ushort;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct WindowClass<'a> {
    atom: NonZero_ATOM,
    instance: &'a Instance,
}

impl<'a> WindowClass<'a> {
    pub unsafe fn from_raw(atom: NonZero_ATOM, instance: &'a Instance) -> WindowClass {
        WindowClass { atom, instance }
    }

    pub fn into_raw(self) -> NonZero_ATOM {
        let atom = self.atom;
        forget(self);
        atom
    }

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
        assert_ne!(ok, 0);
    }
}

#[derive(Educe)]
#[educe(Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Window<'a, 'b, Data> {
    h_wnd: NonNull<HWND__>,
    data: PhantomType<Data>,
    #[educe(PartialOrd(ignore), Ord(ignore), PartialEq(ignore), Eq(ignore), Hash(ignore))]
    class: &'a WindowClass<'b>
}

impl<'a, 'b, Data> Window<'a, 'b, Data> {
    pub unsafe fn from_raw(h_wnd: NonNull<HWND__>, class: &'a WindowClass<'b>) -> Window<'a, 'b, Data> {
        Window {
            h_wnd,
            data: PhantomType::new(),
            class
        }
    }

    pub fn into_raw(self) -> NonNull<HWND__> {
        let h_wnd = self.h_wnd;
        forget(self);
        h_wnd
    }

    pub fn as_h_wnd(&self) -> NonNull<HWND__> { self.h_wnd }

    pub fn class(&self) -> &'a WindowClass<'b> { self.class }

    pub fn new(name: &Nul<u16>, class: &'a WindowClass<'b>, data: Data) -> io::Result<Window<'a, 'b, Data>> {
        let h_wnd = unsafe { CreateWindowExW(
            0,
            class.as_atom().get() as usize as LPCWSTR,
            name.as_ptr(),
            WS_OVERLAPPEDWINDOW,
            CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
            HWND_DESKTOP,
            null_mut(),
            class.instance().as_handle().as_ptr(),
            Box::into_raw(Box::new(data)) as LPVOID
        ) };
        NonNull::new(h_wnd).ok_or_else(io::Error::last_os_error).map(|h_wnd| Window { h_wnd, data: PhantomType::new(), class })
    }

    pub fn show(self) -> bool {
        unsafe { ShowWindow(self.h_wnd.as_ptr(), SW_SHOWDEFAULT) != 0 }
    }
}

unsafe extern "system" fn wnd_proc(h_wnd: HWND, message: UINT, w_param: WPARAM, l_param: LPARAM) -> LRESULT {
    match message {
        WM_DESTROY => {
            PostQuitMessage(0);
            0
        },
        _ => DefWindowProcW(h_wnd, message, w_param, l_param),
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
