program main
    use random
    use net_mod
    implicit none
    real(8), parameter :: pi=acos(-1.d0)
    integer :: spin(N)
    real(8) :: E, M, E2, M1, M2, M4, tmp
    real(8) :: T, beta
    integer :: it, samp=100000
    character(99) :: name
    integer :: fid=10, seed=1

    write(name, "('data/', i3.3, '_', a3, '_', i4.4)") SB, net_type, N
    open(fid, file=trim(name), action='write')

    call init
    do it = 1, samp
        call one_mc(1.d-3)
    end do
    write(fid, "('# init done')")
    write(fid, "('# seed=', i5)") seed
    write(fid, "('# samp=', i5)") samp
    T = 1.d1
    do
        beta = 1.d0 / T
        do it = 1, samp
            call one_mc(beta)
        end do
        E = 0.d0; E2 = 0.d0
        M = 0.d0; M1 = 0.d0; M2 = 0.d0; M4 = 0.d0
        do it = 1, samp
            call one_mc(beta)
            tmp = calc_E()
            E = E + tmp
            E2 = E2 + tmp**2
            tmp = sum(m_map(spin(1:N)))
            M = M + abs(tmp)
            M1 = M1 + tmp
            M2 = M2 + tmp**2
            M4 = M4 + tmp**4
        end do
        call output(fid)
        T = T - 1.d0 / 128
        if (T < 1.d-1) then
            exit
        end if
    end do

contains
    subroutine init
        integer i

        call warmDRN(seed)
        call init_net

        do i = 0, S-1
            m_map(i) = cos(i * 2 * pi / S)
        end do
        do i = -2*S+2, 2*S-2
            e_map(i) = -cos(i * 2 * pi / S)
        end do

        call rfr(N)
        do i = 1, N
            spin(i) = int(S * ur(i))
            if (spin(i) == S) then
                write(*, *) i, ur(i)
            end if
        end do
    end subroutine init

    real(8) function calc_E()
        real(8) :: E
        integer :: i
        E = 0.d0
        do i = 1, N
            E = E + sum(e_map(spin(i) + spin(net(1:D, i))))
        end do
        calc_E = E * 5.d-1
    end function calc_E

    subroutine one_mc(beta)
        real(8), intent(in) :: beta
        integer :: i, j
        real(8) :: a, b
        integer :: new_spin

        call rfr(N*3)
        do i = 1, N
            j = 1 + int(N * ur(3*i))
            new_spin = iand(spin(j) + 1 + int((S-1)*ur(3*i-1)), MASK)
            a = sum(e_map(spin(j) - spin(net(1:D, j))))
            b = sum(e_map(new_spin - spin(net(1:D, j))))
            if (ur(3*i-2) * (1.d0 + exp(-beta*(a-b))) < 1.d0) then
                spin(j) = new_spin
            end if
        end do
    end subroutine one_mc

    subroutine output(fid)
        integer, intent(in) :: fid

        E = E / samp
        E2 = E2 / samp
        M = M / samp
        M1 = M1 / samp
        M2 = M2 / samp
        M4 = M4 / samp
        write(fid, "(i6, 99e24.16)") N, T, &
            & E, E2, M, M1, M2, M4
        flush(fid)
    end subroutine output

end program main
