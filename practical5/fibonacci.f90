! Pointers not explicitly required, adressed passed as arguments by default
! In C, a pointer is a variable which stores the memory address of another
! variable of the same type which has a target attribute 
! Printing a pointer prints the value it points to 
! Loc is used to print the value in fortran

module fib
 contains 
 subroutine fibonacci(a,b)
 
   integer (kind=4), intent(inout) :: a, b
   integer (kind=4) :: next
   
  !a=fn-1 b=fn-2, next=fn
  !On exit: a=fn, b=fn-1
  next=a+b
  b=a
  a=next
 end subroutine fibonacci
end module fib 

program fibonacciseries
use fib
implicit none

!Declaraing and initialising simultaneously
integer (kind=4) :: n, i, f0=0, f1=1

! Enter number
write(6,*), 'enter a positive integer'
read(5,*), n

! check if n is <1
if(n .lt. 1) then 
  write(6,*), 'the number is not positive'
  stop
end if

 write(6,*), 'the fibonacci sequence is:'
 ! Formatted printing, integer and then comma
 write(6, '(i0, a, i0, a)', advance='No')
 do i=2, n
  ! Calling fibonacci subroutine, print using formatted printing
  call fibonacci(f1,f0);
  write(6, '(i0,a)', advance='No'), f1, ', ' 
  ! If remainder 
  if(mod((i+1), 10) .eq. 0) write(6,*) 
 end do
end program fibonacciseries
