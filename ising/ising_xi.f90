program main
    use random
    use net_mod
    implicit none
    integer, parameter :: SEED=10
    integer, parameter :: SAMP=100, MAX_TIME=100, RESOLUTION=4
    integer :: spin(N), spin0(N)
    integer :: E
    real(8) :: T, beta, sig
    real(8) :: gg, g1(0:MAX_TIME), g2(0:MAX_TIME)
    integer :: i, time, tt, ss
    real :: t_start0, t_start, t_end

    call warmDRN(SEED)

    call cpu_time(t_start0)
    do tt = 1, 12*RESOLUTION
        T = tt * 1.d0 / RESOLUTION
        beta = 1.d0 / T
        do ss = 1, 8*RESOLUTION
            call cpu_time(t_start)
            sig = ss * 1.d0 / RESOLUTION
            g1(:) = 0.d0
            g2(:) = 0.d0
            g1(0) = 1.d0
            g2(0) = 1.d0
            do i = 1, SAMP
                call init_spin
                call calc_E(E)
                do time = 1, MAX_TIME
                    call one_mc
                    gg = dot_product(spin0, spin) * 1.d0 / N
                    g1(time) = g1(time) + gg
                    g2(time) = g2(time) + gg*gg
                end do
            end do
            g1(1:) = g1(1:) / SAMP
            g2(1:) = g2(1:) / SAMP
            call cpu_time(t_end)
            call output
        end do
    end do
    call cpu_time(t_end)
    write(*, *) t_end - t_start0
contains
    subroutine output
        integer :: time
        character(99) :: name
        write(name, "('g_time/', a3, '_N', i4.4, '_R', i2.2, '_T', i3.3, '_S', i3.3)") &
                & net_type, N, RESOLUTION, tt, ss
        open(10, file=trim(name), action="write")
        write(10, "('# time =', f10.4)") t_end - t_start
        do time = 0, MAX_TIME
            write(10, "(i5, 3e26.16)") time, g1(time), g2(time)
        end do
        close(10)
    end subroutine output
    
    subroutine init_spin
        call rfr(N)
        spin(1:N) = mod(ir(1:N), 2)*2 - 1
        spin0(:) = spin(:)
    end subroutine init_spin
    
    subroutine one_mc
        integer :: mmc, i, dE
        real(8) :: gauss(N), gE

        call rfr(N*4)
        gauss(1:N) = sqrt(-2.d0*log(1.d0 - ur(1:N))) * sin(6.283185307179586*ur(N+1:N*2))*sig
        do mmc = 1, N
            i = mod(ir(N*2+mmc), N) + 1
            call calc_dE(dE, i)
            gE = dE + gauss(mmc)
            if (gE < 0.d0 .or. ur(N*3+mmc) < exp(-gE*beta)) then
                spin(i) = -spin(i)
                E = E + dE
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
