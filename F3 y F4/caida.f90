program main2

    implicit none
    integer :: i, j, k, l, idx
    integer, parameter :: lado = 120
    integer, parameter :: N = 5*lado*lado/4  ! 18000
    integer, parameter :: pasos = 1000000 ! 1000000
    integer, parameter :: burn = 2000   ! pasos desechados para calcular observables
    integer, parameter :: intentos = 50 !10 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real, parameter :: alpha = 0.2
    real :: lambda = 0.
    real :: gamma = 0.16
    integer, parameter :: numgamma = 1    ! numero de gammas que se muestrean
    real :: kmeansq = 0.
    real :: kmean = 0.
    real, parameter :: sigmae = 3.4
    real, parameter :: sigmai = 2.3
    real, parameter :: csige = 1.  ! constante que multiplica a función de conectividad
    real, parameter :: csigi = 1.
    real, parameter :: we = 1.      ! pesos absolutos (sin signo, éste va en la etiqueta)
    real, parameter :: wi = 2.5
    real*8 :: dist
    real*8 :: distx
    real*8 :: disty
    real*8 :: rand1
    real*8 :: rand2
    real*8 :: coord(N,3)
    real :: prjen = 0.
    integer :: nn(N, 200) = 0
    integer :: nnnumber(N) = 0
    integer :: counter(N) = 1
    integer :: etiq(N) = 1
    integer :: status(N) = 0
    integer :: statuspre(N) = 0
    real*8 :: ro (pasos) = 0.
    real*8 :: rotot (pasos) = 0.
    real :: amean = 0.               ! actividad media
    real :: mamean = 0.         ! <Amean> media en los 30 intentos
    integer :: count = 1
    real, parameter :: pi = 3.141592653
    real, parameter :: pt = 1.0     ! probabilidad de teletransportar neurona
    real :: T1, T2, T3
    real, parameter :: U = 0.07
    real, parameter :: taurec = 50
    character (len=15)::name
    character(len=4)::argumento
    integer::argint

    call get_command_argument(1,argumento)
    read(argumento,*) argint
    write(name, '("ro",I5,".txt")') argint

    open(1,status='replace', file=name)
    do l = 1, numgamma

        gamma = 1.25
        write(*,*) 'gamm', gamma, 'sigE', sigmae, 'sigI', sigmai, 'we', we, 'wi', wi, 'pt', pt, 'epsil'

        mamean = 0.
        write(*,*) "iteracion ", l, "    gamma = ", gamma
        do idx = 1, intentos
            call cpu_time(T1)

            ! ---INICIALIZACION---
            call init_random_seed()
            amean = 0.
            kmeansq = 0.
            kmean = 0.
            write(*,*) "intento",idx
            do i = 1, N
                status(i) = 0
                statuspre(i) = 0
                nnnumber(i) = 0
                etiq(i) = 1
                counter(i) = 1
                do j =1, 200
                    nn(i,j) = 0
                enddo
            enddo
            do i = 1, pasos
                ro(i) = 0.
            enddo

            ! ---TOPOLOGIA---
            ! excitadoras
            count = 1
            do i = 0, (lado-1)
                do j = 0, (lado-1)
                    coord(count,1) = float(i)
                    coord(count,2) = float(j)
                    etiq(count) = 1
                    count = count + 1
                enddo
            enddo
            !inhibidoras
            do i = 0, (lado/2 - 1)
                do j= 0, (lado/2 - 1)
                    coord(count,1) = 0.5 + float(i)*2.
                    coord(count,2) = 0.5 + float(j)*2.
                    etiq(count) = -1
                    count = count + 1
                enddo
            enddo
            ! teletransporte
            do i = 1, N
                call random_number(rand1)
                if (rand1<pt) then
                    call random_number(rand1)
                    call random_number(rand2)
                    coord(i,1) = rand1 * float(lado)
                    coord(i,2) = rand2 * float(lado)
                endif
            enddo

            do i = 1, N
                call random_number(rand1)
                if (rand1<0.7) then
                    status(i) = 1
                    statuspre(i) = 1
                endif
            enddo
            do i = 1, N
                do j = 1, N
                    if (i/=j) then 
                        distx=min( (max(coord(i,1),coord(j,1))-min(coord(i,1),coord(j,1))) , &
                        (float(lado)+min(coord(i,1),coord(j,1))-max(coord(i,1),coord(j,1))) )
                        disty=min( (max(coord(i,2),coord(j,2))-min(coord(i,2),coord(j,2))) , &
                        (float(lado)+min(coord(i,2),coord(j,2))-max(coord(i,2),coord(j,2))) )
                        dist = sqrt(distx**2. + disty**2.)
                        ! call random_number(rand1)
                        if (etiq(i) == 1) then
                            if (dist<sigmae) then
                                nn(j,counter(j)) = i  
                                nnnumber(j) = nnnumber(j) + 1
                                counter(j) = counter(j) + 1
                            endif
                        else
                            if (dist<sigmai) then
                                nn(j,counter(j)) = i
                                nnnumber(j) = nnnumber(j) + 1
                                counter(j) = counter(j) + 1
                            endif
                        endif
                    endif
                enddo
            enddo
            do i = 1, N
                kmeansq = kmeansq + float(nnnumber(i))
            enddo
            kmeansq = kmeansq / float(N)
            write(*,*) "kmean=", kmeansq
            ! kmeansq = sqrt(kmeansq)
            call cpu_time(T2)
            write(*,*) 'tiempo1', floor((T2-T1)/60), ':', mod(floor(T2-T1), 60)
            ! ---DINAMICA---
            do i = 1, pasos
                do j = 1, N
                    lambda = 0.
                    do k = 1, nnnumber(j)
                        if (nn(j,k)==0) exit
                        if (etiq(nn(j,k))==1) then  ! si el vecino k-esimo es excitador
                            lambda = lambda + float(etiq(nn(j,k)) * statuspre(nn(j,k))) * we! * x(nn(j,k))
                        else
                            lambda = lambda + float(etiq(nn(j,k)) * statuspre(nn(j,k))) * wi! * x(nn(j,k))
                        endif
                    enddo
                    lambda = lambda * gamma / float(nnnumber(j))
                    if (statuspre(j)==0) then
                        if (lambda<0) then
                            prjen = 0.
                        else if (lambda<=1.) then
                            prjen = lambda
                        else
                            prjen = 1.
                        endif
                        call random_number(rand1)
                        if (rand1<prjen) then
                            status(j) = 1
                        endif
                    else
                        if (lambda<0) then
                            prjen = 1.
                        else if (lambda<=1.) then
                            prjen = 1.-lambda
                        else
                            prjen =0
                        endif
                        call random_number(rand1)
                        if (rand1<(prjen)) then
                            status(j) = 0
                        endif
                    endif

                enddo
                do j = 1,N
                    ro(i) = ro(i) + float(status(j))
                    statuspre(j) = status(j)
                enddo
                if (ro(i)==0.) then
                    write(*,*) "cae"
                    write(*,*) i
                    exit
                endif
            enddo   ! fin un periodo

            ! CALCULOS FINALES PARA 1 REALIZACION (1 RED, 1 )

            do i = 1, pasos
                ro(i) = ro(i) / float(N)
                if (i>burn) then
                    amean = amean + ro(i)
                endif
                rotot(i) = rotot(i) + ro(i)
            enddo
            amean = amean / (float(pasos - burn))
            ! write(0,*) gamma, kmean, amean, desva, cv
            write(*,*) "amean", amean
            

            mamean = mamean + amean
            call cpu_time(T3)
            write(*,*) 'tiempo2', floor((T3-T2)/60), ':', mod(floor(T3-T2), 60)
        enddo ! fin intentos para un gamma
        do i = 1, pasos
            write(1,*) i, rotot(i)/float(intentos)
            if (rotot(i)==0) then
                exit
            endif
        enddo
        mamean = mamean / float(intentos)


    enddo ! fin todos los gamma
end program

SUBROUTINE init_random_seed()
    INTEGER :: i, n, clock
    INTEGER, DIMENSION(:), ALLOCATABLE :: seed
  
    CALL RANDOM_SEED(size = n)
    ALLOCATE(seed(n))
  
    CALL SYSTEM_CLOCK(COUNT=clock)
  
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    CALL RANDOM_SEED(PUT = seed)
  
    DEALLOCATE(seed)
  END SUBROUTINE