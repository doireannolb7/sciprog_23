! Module with parameters (constants)
module constants
  integer (kind=4), parameter :: N = 12
  real (kind=4) :: pi
end module constants

! Module with subroutines
module subroutines
  interface
    subroutine degtorad(deg, rad)
      use constants
      real(kind=4), intent(in) :: deg
      real(kind=4), intent(out) :: rad
    end subroutine degtorad
    subroutine trapezoidalrule(TanArr, area)
      use constants
      real(kind=4), intent(in) :: TanArr(N+1)
      real(kind=4), intent(out) :: area
    end subroutine trapezoidalrule
  end interface
end module subroutines

program Trapezoid
  use constants
  use subroutines
  implicit none
  integer (kind=4) :: i
  real (kind=4) :: TanArr(N+1), deg, rad, area, mult_rad

  ! Calculate value of pi 
  pi = atan(1.0) * 4.0

  ! Using do loop to calculate values of f(xi) from i=0 to 12
  ! Need radians for tan fucntion 
  ! Using degtorad subroutine to convert degrees to radians
  ! Answers (tan (xi)) are stored in tan array
  ! Priting answers to screen
  do i=1,N+1
    deg = (i-1)*5.0
    call degtorad(deg, rad)
    TanArr(i) = tan(rad)
    write(6,*) 'TanArr[', i, ']= ', TanArr(i), '(f(x) at x= ', i, ')'
  end do

 ! Using trapezoidal rule subroutine 
 ! tan array values as inputs, areas as outputs
  call trapezoidalrule(TanArr, area)

  ! Approx result
  write(6,*) ' Trapezoidal result is ',area
  ! Real result 
  ! Integral of tan is ln(2)
  write(6,*) ' Real result is ',log(2.0)

    stop
end program 

! Subroutine to convert degrees to radians
subroutine degtorad(deg, rad)
  use constants
  real(kind=4), intent(in) :: deg
  real(kind=4), intent(out) :: rad
  rad=(pi*deg)/180.0
end subroutine degtorad

! Subroutine to calculate area values from i=0 to i=12
subroutine trapezoidalrule(TanArr, area)
  use constants
  real(kind=4), intent(in) :: TanArr(N+1)
  real(kind=4), intent(out) :: area
  real(kind=4) :: mult_rad

  ! Apply formula
  ! Find the area by trapezodial rule
  ! Add areas of end points
  area = TanArr(1)+TanArr(N+1);
  ! Calculate the area at points 1-11 and add them up
  ! Next, 2 times f(x1)+ 2 times f(x2)...+ 2 timesf(xN-1)
  ! as in the formula using loops 
  do i=2, N, 1
    area=area+2.0*TanArr(i)
  end do
 
  ! Multiply area by (pi/3)/2(12) after converting it to radians 
  call degtorad(((60.0-0.0)/(2.0*N)), mult_rad)
  area=mult_rad*area;
end subroutine trapezoidalrule
