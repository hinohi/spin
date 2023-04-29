program main
    use random
    use net_mod
    implicit none
    integer :: spin(N)
    integer, parameter :: SEED=10
    integer, parameter :: SAMP=10000
    integer :: E, M
    real(8) :: T, E1, E2, M0, M1, M2, M4
    integer :: i
    real :: t_start0, t_start, t_end
    character(99) :: name

    write(name, "('data/', a2, '_', a3, '_N', i4.4)") "gd", net_type, N
    open(10, file=trim(name), action="write")
    
    call warmDRN(SEED)
    write(10, "('# seed = ', i4)") SEED
    write(10, "('# samp = ', i5)") SAMP
    call init_spin
    
    call calc_E(E)
    M = sum(spin(:))
    T = 15.d0
    call cpu_time(t_start0)
    do
        call cpu_time(t_start)
        if (T < 1.d0) exit
        T = T - 1.d0 / 16
        do i = 1, SAMP
            call one_mc
        end do
        E1 = 0.d0
        E2 = 0.d0
        M0 = 0.d0
        M1 = 0.d0
        M2 = 0.d0
        M4 = 0.d0
        do i = 1, SAMP
            call one_mc
            E1 = E1 + E*1.d0/N
            E2 = E2 + (E*1.d0/N)**2
            M0 = M0 + M*1.d0/N
            M1 = M1 + abs(M*1.d0/N)
            M2 = M2 + abs(M*1.d0/N)**2
            M4 = M4 + abs(M*1.d0/N)**4
        end do
        call cpu_time(t_end)
        call output
    end do
contains
    subroutine output
        write(10, "(8e26.16)") &
                & T, E1/SAMP, E2/SAMP, & 
                & M0/SAMP, M1/SAMP, M2/SAMP, M4/SAMP, &
                & (t_end-t_start)/SAMP
    end subroutine output
    
    subroutine init_spin
        call rfr(N)
        spin(1:N) = mod(ir(1:N), 2)*2 - 1
    end subroutine init_spin
    
    subroutine one_mc
        integer :: mmc, i, dE
        real*8 :: gauss(N)

        call rfr(N*3)
        gauss(1:N) = sqrt(-2.d0*log(1.d0 - ur(1:N))) * sin(6.283185307179586*ur(N+1:N*2))*T
        do mmc = 1, N
            i = mod(ir(mmc+N*2), N) + 1
            call calc_dE(dE, i)
            if (dE < gauss(mmc)) then
                spin(i) = -spin(i)
                E = E + dE
                M = M + spin(i)*2
            end if
        end do
    end subroutine one_mc

    subroutine calc_E(E)
        integer, intent(out) :: E
        integer :: i
        
        E = 0
        do i = 1, N
            E = E - spin(i) * sum(spin(net(1:D, i)))
        end do
        E = E / 2
    end subroutine calc_E
    
    subroutine calc_dE(dE, i)
        integer, intent(out) :: dE
        integer, intent(in) :: i
        dE = spin(i) * sum(spin(net(1:D, i))) * 2
    end subroutine calc_dE

end program main
