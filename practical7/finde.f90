! Not using pointers here, allocating array automatically does this
! Find e using the Taylor series expansion feom e^x

Program finde
  implicit none
  interface
    function factorial(n)
       integer (kind=4) :: factorial, n 
    end function
 end interface
 integer (kind=4) :: i, order, ierr
 real (kind=8) :: e
 real (kind=8), allocatable :: terms(:) 

! Enter polynomial order
 write(6,*), 'Enter required polynomial order'
 read(5,*,iostat=ierr) order
 if(ierr .ne. 0) write(6,*) 'Problem with input'

 allocate(terms(order))

  do i=1, order
  terms(i)=1.0/real(factorial(i), kind=8)
  write(6, '(a,i2,a,f20.16)'), 'e term for order', i, 'is', terms(i)  
 end do

! Type casting 
! Shortcut to sum terms array is used instead of loop
! Then, finding difference of approxiamated and true value
 e=dble(1.0)
write(6,*) 'e is estimated as ', e+sum(terms), 'Difference is: ', e+sum(terms)-dexp(e)

! Must free some space
 deallocate(terms)  

 stop
end program finde

! Specifying return value
integer (kind=4) function factorial(n)
  implicit none 
  integer (kind=4), intent(in) :: n
  integer (kind=4) :: i,x
 
! Using a loop to calculate factorial value to return it to main program
  x=1 
  do i=1, n
   x=x*i
 end do
 factorial = x
 return  

end function factorial

! It is found that the values become less accurate after the 13th term
! This is due to integer overflow, i.e., integers cannot be used to represent
! higher polynomial orders in 4 bytes
