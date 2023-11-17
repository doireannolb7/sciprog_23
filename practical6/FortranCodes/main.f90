module functions 
  interface  
     function matmult(n, p, q, a, b) result(c)
     integer (kind=4) :: i, j, k
     integer (kind=4) :: n, p, q
     real (kind=8), intent (in) :: a(n,p), b(p,q)
     real (kind=8) :: c(n,q) 
     end function matmult
  end interface
end module functions

program matrixmult
!Declare variables
use functions 
integer (kind=4), parameter ::  n=5, p=3, q=4
! n,p,q numbers of rows/columns
real (kind=8) :: a(n,p), b(p,q), c(n,q)
integer (kind=4) :: i, j, k


!initialise A,B,C
! Initialise A
  do i=1, n
   do j=1, p
     a(i,j)=i+j
   end do
 end do
! Initialise B
  do i=1, p
    do j=1, q
      b(i,j)=i-j
   end do
 end do
! Initialise C (matrix of all zeros)
c = 0.0

! Perform matrix multiplication
c=matmult(n,p,q,a,b)

! Print matrices
 write(6,*) 'This is matrix A'
 do i=1, n
   do j=1, p
   ! float with 3 digits, no decimal place
   ! Advance no, want to print for each i (ith row)
   write(6, '(f3.0)', advance='no') a(i,j)
   end do
   write(6,*)
 end do

 write(6,*) 'This is matrix B'
 do i=1, p
    do j=1, q
    write(6, '(f3.0)', advance='no') b(i,j)
  end do
  write(6,*)
 end do

 write(6,*) 'This is matrix C'
  do i=1,n
    do j=1,q
    write(6, '(f4.0)', advance='no') c(i,j)
  end do
  write(6,*)
 end do

end program matrixmult
