 Module Constants
   integer (kind=4), parameter :: length = 1000
 end module constants

Module hypob
 
   contains
! Using funcion here rather than a subroutine
! Calculates inverse hyperbolic tangent of real 'x'
! With a precision error of less than 'delta'
   function arctanh(x,delta)
       implicit none
       real (kind=8) :: arctanh, x, delta
       integer (kind=4) :: n
       real (kind=8) :: arctan, elem, val
       arctan = x; n = 1; 

       elem = x
       do while (abs(elem) .ge. delta)
          val = 2 * n + 1
          elem = x**val/val
          arctan = arctan + elem
          n = n +1
      end do 
      arctanh = arctan
      ! Return used, value calculated is returned to function name
      ! Only one value returned 
      return
   end function arctanh

!Calculates inverse hyperbolic tangent of real 'x'
   function arctanh2(x)
   implicit none 
   real (kind=8) :: arctanh2, x
   arctanh2 = (log(1.0+x) - log(1.0-x))/2
   ! Returning value to arctanh2
   return
 end function arctanh2
end module 


Program invhypo
    ! Using two modules, one with constants and one with the two functions
    use constants
    use hypob
    implicit none
    real (kind=8) :: x, delta, tan1(length), tan2(length)
    integer (kind=4) :: j=1
!REading the precision to be applied to the numerical method
    write(6,*) ' Enter the method''s precision in (0.1); '
    read(5,*) delta
   
    x=-0.9
    do while(x .le. 0.9 .AND. j .le. length)
    ! Calling both functions
    tan1(j) = arctanh(x,delta)
    tan2(j) = arctanh2(x)
    write(6,fmt='(a, f20.10,a,f20.10)') 'Difference at ', x, ' is ', abs(tan1(j) - tan2(j))
    j=j+1
    x=x+0.1
  end do 
  
  stop
end program invhypo 
