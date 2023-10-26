program conversion
   implicit none
! Declare variables 
   integer (kind=4) :: i,inum,tmp,numdigits
   character (len=60) :: binnum
   real(kind=4) :: fnum
   character (len=32) :: binnum_reverse
   integer (kind=4) ::remainder, length, j

! Intialise 4-byte integer
   inum = 33554431
! Convert to 4-byte float
   fnum = real(inum)       

! Binary converter using modulus   
   i=1
   tmp=inum
   do while (tmp > 0)
      if(mod(tmp,2)==0) then
        binnum(i:i) = '0'
        else
             binnum(i:i) = '1'
      end if
      tmp = tmp/2
      i = i+1
   end do
! Remove blank trailing characters in string 
   binnum=trim(binnum)

! Reverse string
   length = len(binnum)

    do i = 1,length

        j = length - i + 1

        binnum_reverse(i:i) = binnum(j:j)

    end do

! TODO Complete the expression
   numdigits = nint(log(fnum)/log(2.0))
 
!  write(6,*) ' The bumber of digits is ', numdigits
   write(6,*) ' The number of digits is ', numdigits

   write(6,'(a,i0,a,f0.1,a,b0)')  'inum=',inum,', fnum=',fnum,', inum in binary=',inum


end program conversion
