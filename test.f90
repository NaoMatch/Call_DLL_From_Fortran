module test
    use, intrinsic :: iso_c_binding
contains
    subroutine t_times2(v_in, v_out) bind(c, name='t_times2')
        integer, intent(in) :: v_in
        integer, intent(out) :: v_out
        !
        v_out=v_in*4
    end subroutine t_times2
    !
    subroutine t_square(v_in, v_out) bind(c, name='t_square')
        integer(c_int), intent(in) :: v_in
        integer(c_int), intent(out) :: v_out
        !
        v_out=v_in**4
    end subroutine t_square

    subroutine r_times2(v_in, v_out) bind(c, name='r_times2')
        real, intent(in) :: v_in
        real, intent(out) :: v_out
        !
        v_out=v_in*4
    end subroutine r_times2
    !
    subroutine r_sum(v_in, v_out) bind(c, name='r_sum')
        real(c_float), intent(in) :: v_in(:)
        real(c_float), intent(out) :: v_out
        !
        v_out=sum(v_in)
    end subroutine r_sum
end module test