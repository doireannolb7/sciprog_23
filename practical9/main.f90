  
include 'magic_square.fh'

program magsq
! Using module from header file    
    use msquare
    implicit none
    integer (kind=4) n,i,ierr,num(100)
    integer (kind=4), allocatable :: magicSquare(:,:)
    character (len=6) :: text
    character (len=100) :: filename, line

! Code for getting values from a file 

! Open the file contaiing matrix
  write(6, *) ' enter the file name with the square matrix '
  read(5, *) filename
  

! Allocating a matrix for storing the magic square
! as an array of pointers, where each pointer is a row
   ierr=0
   open(unit=1, file=filename, status='OLD', form='formatted', &
       & access='sequential', action='READ', iostat=ierr)
   if(ierr .ne. 0) then 
     write(6, *) 'Sorry cannot open file', filename
  end if 


!  Read in the rows from each line
 
  n=0
    do
      read(1, *,iostat=ierr)
      if(ierr/=0) exit
      n=n+1
    end do 
    rewind(unit=1)
    write(6, *), n
    
    allocate(magicSquare(n,n)) 


! Inputting integer data into the matrix
    do i = 1, n
       read(1,*,iostat=ierr) magicSquare(i,:)
    end do
     
    if(isMagicSquare(magicSquare, n)) then 
      text = 'is'
     else
      text='is NOT'
   end if 
  write(6,*), 'This square ', trim(text), 'magic'


! Freeing each row separately before freeing the array of pointers
  deallocate(magicSquare)

  close(unit=1, status='keep')
end program magsq

