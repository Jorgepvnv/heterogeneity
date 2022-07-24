program main2

    implicit none
    integer :: i, j, k, l, idx
    integer, parameter :: lado = 120
    integer, parameter :: N = 5*lado*lado/4  ! 18000
    integer, parameter :: pasos = 80000     ! 12.000 1 h 30
    integer, parameter :: burn = 8000   ! pasos desechados para calcular observables
    integer, parameter :: intentos = 60 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real, parameter :: alpha = 0.2
    real :: lambda = 0.
    real :: gamma = 0.16   
    integer, parameter :: numgamma = 1    ! numero de gammas que se muestrean
    real :: kmeansq = 0.
    real, parameter :: sigmae = 3.4   ! CON
    real, parameter :: sigmai = 2.3
    real, parameter :: we = 1.      ! pesos absolutos (sin signo, éste va en la etiqueta)
    real, parameter :: wi = 2.5
    integer, parameter :: muestravar = 10000   ! numero de neuronas que se muestrean para calcular cv
    ! integer :: isi(muestravar,pasos/2) = 0           ! inter-spike intervals (se guardan para MUESTRAVAR neuronas)
    ! integer :: tlastsp(muestravar) = 0              ! tiempo desde el último spike (igualmente para MUESTRAVAR)
    ! real :: isimean(muestravar) = 0.
    ! real :: isidesv(muestravar) = 0.
    ! real :: cvind(muestravar) = 0.                 ! coeficientes de variacion individuales
    ! integer :: countsp(muestravar) = 0             ! array para guardar cuántos spike lleva cada neurona
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
    real :: cv = 0.                  ! coef variacion
    real :: amean = 0.               ! actividad media
    real :: mamean = 0.         ! <Amean> media en los 30 intentos
    real :: mcv = 0.            ! coef variacion medio
    real :: varcv = 0.
    integer :: count = 1
    real, parameter :: pi = 3.141592653
    real, parameter :: pt = 0.2    ! probabilidad de teletransportar neurona
    character (len=15)::name
    character(len=4)::argumento
    integer::argint
    real :: cE
    real :: cI
    integer :: ruido(lado**2) = 0   ! ruido externo (solo a excitadoras)
    integer, parameter :: numex = 30    !numero de externas para cada E
    real :: r = 0.2     ! numero medio de inputs excitadores externos
    real :: pex = 0     ! probabilidad de que externa esté activa
    real :: bajo1 = 0.05
    real :: alto1 = 0.09
    real :: bajo2 = 2.7
    real :: alto2 = 3.4
    
    call get_command_argument(1,argumento)
    read(argumento,*) argint
    write(name, '("r",I2,".txt")') argint
    open(0,status='replace', file=name)

    do l = 1, numgamma
        if (argint<10) then
            r = (bajo1)*(alto1/bajo1)**(float(argint)/10)
        else
            r = (bajo2)*(alto2/bajo2)**(float(argint)/10)
        endif
        ! r = 10.
        gamma = 1.55
        mamean = 0.
        mcv = 0.
        pex = r / float(numex)
        write(*,*) "iteracion", l, "gamma = ", gamma, "r = ", r
        do idx = 1, intentos

            ! ---INICIALIZACION---
            call init_random_seed()
            amean = 0.
            kmeansq = 0.
            cv = 0.
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
            ! do i = 1, muestravar
            !     tlastsp(i) = 0
            !     isimean(i) = 0.
            !     isidesv(i) = 0.
            !     cvind(i) = 0.
            !     countsp(i) = 0 
            !     do j = 1, pasos/2
            !         isi(i,j) = 0 
            !     enddo
            ! enddo
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
                        if (etiq(i) == 1) then
                            if (dist<sigmae) then
                                nn(j,counter(j)) = i        ! la lista de vecinos muestra cuales introducen input a la neurona, no a cuales proyecta
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

            ! ---DINAMICA---
            do i = 1, pasos
                do j = 1, N
                    if (etiq(j)==1) then
                        ruido(j) = 0
                        do k = 1,numex
                            call random_number(rand1)
                            if (rand1<=pex) then
                                ruido(j)=ruido(j)+1
                            endif
                        enddo
                    endif
                    lambda = 0.
                    do k = 1, nnnumber(j)
                        if (nn(j,k)==0) exit
                        if (etiq(nn(j,k))==1) then  ! si el vecino k-esimo es excitador
                            cE = float(etiq(nn(j,k)) * statuspre(nn(j,k))) * we
                            lambda = lambda + cE
                        else
                            cI = float(etiq(nn(j,k)) * statuspre(nn(j,k))) * wi
                            lambda = lambda + cI
                        endif
                    enddo
                    if (etiq(j)==1) then
                        lambda = lambda + ruido(j)      ! se introduce el ruido externo
                    endif
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
                            ! if ((j<=muestravar).and.(countsp(j)/=0).and.(i>burn)) then
                            !     isi(j,countsp(j)) = tlastsp(j)
                            !     countsp(j) = countsp(j)+1
                            !     tlastsp(j) = 0
                            ! else if ((j<=muestravar).and.(countsp(j)==0).and.(i>burn)) then
                            !     countsp(j) = 1
                            !     tlastsp(j) = 0
                            ! endif
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
                    ! if (j<3) then
                    !     write(*,*) Iad(j)
                    ! endif
                enddo
                do j = 1,N
                    ro(i) = ro(i) + float(status(j))
                    statuspre(j) = status(j)
                enddo
                !                   QUITO EL EXIT DE LA CAIDA PARA CALCULAR MEDIA
                ! if (ro(i)==0.) then
                !     write(*,*) "cae"
                !     write(*,*) i
                !     meanfall = meanfall + i
                !     countfall = countfall + 1
                !     fallvector(idx) = i
                !     exit
                ! endif
            enddo   ! fin un periodo

            ! CALCULOS FINALES PARA 1 REALIZACION (1 RED, 1 )
            ! do i=1, muestravar
            !     if (countsp(i)>1) then
            !         do j=1, (countsp(i)-1)
            !             isimean(i) = isimean(i) + float(isi(i,j))
            !         enddo
            !         isimean(i) = isimean(i) / float(countsp(i)-1)
            !     else
            !         isimean(i) = 0.
            !     endif
            ! enddo

            ! do i=1, muestravar
            !     if (countsp(i)>1) then
            !         do j=1, (countsp(i)-1)
            !             isidesv(i) = isidesv(i) + (float(isi(i,j))-isimean(i))**2.0
            !         enddo
            !         isidesv(i) = sqrt(isidesv(i) / float(countsp(i)-1))
            !         cvind(i) = isidesv(i) / isimean(i)
            !     else
            !         cvind(i) = 0
            !     endif
            !     ! if (cvind(i)/=0) then
            !     ! write(1,*) cvind(i)         ! CUENTO TAMBIEN LOS QUE SON 0
            !     ! endif
            !     cv = cv + cvind(i)
            ! enddo
            ! write(*,*) cvind
            cv = cv / float(muestravar)

            do i = 1, pasos
                ro(i) = ro(i) / float(N)
                ! write(1,*) i, ro(i)
                if (i>burn) then
                    amean = amean + ro(i)
                endif
            enddo

            amean = amean / (float(pasos - burn))
            ! write(0,*) gamma, kmean, amean, desva, cv
            write(*,*) "amean", amean
            write(*,*) "CV", cv

            mamean = mamean + amean
            mcv = mcv + cv
        enddo ! fin intentos para un gamma
        mamean = mamean / float(intentos)
        mcv = mcv / float(intentos)
        !write(*,"(a)") achar(7)
        write(0,*) gamma, r, mamean

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