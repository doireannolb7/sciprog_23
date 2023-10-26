
program sum
! Declare variables
   integer (kind=4) :: i
   real (kind=4) :: sum1, sum2, x
   

! First sum
   sum1 = 0.0
   do i=1,1000,1
      !  Insert here
      x = i
     sum1 = sum1 + 1.0/x     
   end do


! Second sum 
   sum2 = 0.0
   do i=1000,1,-1
      ! Insert the same line as above except use sum2
      x = i
      sum2 = sum2 + 1.0/x
   end do

! Find the difference

   write(6,*) ' Difference between the two is ',sum1-sum2

end program sum
