program noncontig

#ifndef MPIF08
  use mpi

  implicit none

  integer :: request
  integer, dimension(MPI_STATUS_SIZE) :: status
#else
  use mpi_f08

  implicit none

  type(MPI_Request) :: request
  type(MPI_Status)  :: status
#endif

  integer, parameter :: nmax = 4*1024*1024

  double precision, dimension(nmax) :: sendbuf, recvbuf

  integer :: rank, ierr, i, n

  call MPI_Init(ierr)

  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  if (rank == 0) then
     write(*,*) "MPI_SUBARRAYS_SUPPORTED = ", MPI_SUBARRAYS_SUPPORTED
  end if

  n = 8
  
  do while (n <= nmax)

     do i = 1, nmax

        sendbuf(i) = float(i)
        recvbuf(i) = 0.0

     end do

     if (rank == 0) then

        call MPI_Issend(sendbuf(1:n:2), (n+1)/2, MPI_DOUBLE_PRECISION, 1, &
             0, MPI_COMM_WORLD, request, ierr)

        call MPI_Barrier(MPI_COMM_WORLD, ierr)

        call MPI_Wait(request, status, ierr)

     else if (rank == 1) then

        write(*,*) "n = ", n

        call MPI_Barrier(MPI_COMM_WORLD, ierr)

        call MPI_Recv(recvbuf, (n+1)/2, MPI_DOUBLE_PRECISION, 0, &
             0, MPI_COMM_WORLD, status, ierr)

!        write(*,*) "Using subarrays in Issend: recvbuf = ", recvbuf(1:n)

        if (any(recvbuf(1:(n+1)/2) .ne. sendbuf(1:n:2))) then
           write(*,*) "Using subarrays in Issend: FAIL"
        else
           write(*,*) "Using subarrays in Issend: PASS"
        end if

     else

        call MPI_Barrier(MPI_COMM_WORLD, ierr)

     end if

     do i = 1, nmax

        sendbuf(i) = float(i)
        recvbuf(i) = 0.0

     end do

     ! optionally omit the irecv test as it can cause a segv by
     ! writing over unallocated meory

     ! n = 2*n
     ! cycle

     if (rank == 0) then

        call MPI_Barrier(MPI_COMM_WORLD, ierr)

        call MPI_Ssend(sendbuf, (n+1)/2, MPI_DOUBLE_PRECISION, 1, &
             0, MPI_COMM_WORLD, ierr)

     else if (rank == 1) then

        call MPI_Irecv(recvbuf(1:n:2), (n+1)/2, MPI_DOUBLE_PRECISION, 0, &
             0, MPI_COMM_WORLD, request, ierr)

        call MPI_Barrier(MPI_COMM_WORLD, ierr)

        call MPI_Wait(request, status, ierr)

!        write(*,*) "Using subarrays in Irecv:  recvbuf = ", recvbuf(1:n)

        if (any(recvbuf(1:n:2) .ne. sendbuf(1:(n+1)/2))) then
           write(*,*) "Using subarrays in Irecv:  FAIL"
        else
           write(*,*) "Using subarrays in Irecv:  PASS"
        end if

     else

        call MPI_Barrier(MPI_COMM_WORLD, ierr)

     end if

     n = 2*n

  end do

  call MPI_Finalize(ierr)

end program noncontig
