program uinput

integer (kind=4) :: i, ierr=3
real (kind=8) :: a

! * used which means format isnt specified
! iostat=ierr used to check for errors
! When ierr=0, read statement is executed succesfully
! ierr must be initialised not as zero
! Unit 5 is coming from screen
 write(6,*) ' Enter an int and double '
 do while (ierr .ne. 0)
   read(5,*,iostat=ierr) i,a
 end do 
 if (ierr .ne. 0) write(6,*) ' problem with input '

end program uinput
