program example
    use :: iso_c_binding
implicit none

    integer(c_int), parameter :: rtld_lazy=1 ! value extracte from the C header file
    integer(c_int), parameter :: rtld_now=2 ! value extracte from the C header file
    !
    ! interface to linux API
    interface
        function dlopen(filename,mode) bind(c,name="dlopen")
            ! void *dlopen(const char *filename, int mode);
            use iso_c_binding
            implicit none
            type(c_ptr) :: dlopen
            character(c_char), intent(in) :: filename(*)
            integer(c_int), value :: mode
        end function

        function dlsym(handle,name) bind(c,name="dlsym")
            ! void *dlsym(void *handle, const char *name);
            use iso_c_binding
            implicit none
            type(c_funptr) :: dlsym
            type(c_ptr), value :: handle
            character(c_char), intent(in) :: name(*)
        end function

        function dlclose(handle) bind(c,name="dlclose")
            ! int dlclose(void *handle);
            use iso_c_binding
            implicit none
            integer(c_int) :: dlclose
            type(c_ptr), value :: handle
        end function
    end interface

    ! Define interface of call-back routine.
    abstract interface
        subroutine called_proc (i, i2) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int), intent(in) :: i
            integer(c_int), intent(out) :: i2
        end subroutine called_proc
    end interface

    abstract interface
        subroutine called_proc_2 (r, r2) bind(c)
            use, intrinsic :: iso_c_binding
            real(c_float), intent(in) :: r
            real(c_float), intent(out) :: r2
        end subroutine called_proc_2
    end interface

    abstract interface
        subroutine called_proc_3 (r, r2) bind(c)
            use, intrinsic :: iso_c_binding
            real(c_float), intent(in) :: r(:)
            real(c_float), intent(out) :: r2
        end subroutine called_proc_3
    end interface

    ! testing the dynamic loading
    integer i, i2
    real r, r2
    real v(10), v_sum
    type(c_funptr) :: proc_addr
    type(c_ptr) :: handle
    ! character(256) :: pName, lName

    procedure(called_proc), bind(c), pointer :: proc
    procedure(called_proc_2), bind(c), pointer :: proc_2
    procedure(called_proc_3), bind(c), pointer :: proc_3
    !
    i = 15
    r = 0.50
    call random_number(v)

    call system("gfortran -c test.f90")
    call system("gfortran  -shared -o test.so test.o")

    handle=dlopen("./test.so"//c_null_char, RTLD_LAZY)
    if (.not. c_associated(handle))then
        print*, 'Unable to load DLL ./test.so'
        stop
    end if

    !
    proc_addr=dlsym(handle, "t_times2"//c_null_char)
    if (.not. c_associated(proc_addr))then
        write(*,*) 'Unable to load the procedure t_times2'
        stop
    end if
    call c_f_procpointer( proc_addr, proc )
    call proc(i,i2)
    write(*,*) "t_times2, i2=", i2, i

    !
    proc_addr=dlsym( handle, "r_times2"//c_null_char )
    if ( .not. c_associated(proc_addr) )then
        write(*,*)'Unable to load the procedure r_times2'
        stop
    end if
    call c_f_procpointer(proc_addr, proc_2)
    call proc_2(r,r2)
    write(*,*) "r_times2, r2=", r,r2

    !
    proc_addr=dlsym( handle, "r_sum"//c_null_char )
    if ( .not. c_associated(proc_addr) )then
        write(*,*)'Unable to load the procedure r_sum'
        stop
    end if
    call c_f_procpointer(proc_addr, proc_3)
    call proc_3(v,v_sum)
    write(*,*) "r_sum, v_sum=", v,v_sum, sum(v)

    i = dlclose(handle)
    print*, c_associated(handle)
contains
end program example