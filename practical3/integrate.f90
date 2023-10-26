program integrate
 
 !Declare variables
 integer (kind=4) :: N=12, i
 real (kind=4) :: a=0.0, b_degrees=60.0
 real (kind=4) :: pi, b_radians, area, mult_radians

 ! Must calculate pi and convert degrees to radians
 !Reals are used to avoid integer division 
 pi = atan(1.0) *  4.0
 b_radians=(pi*b_degrees)/180.0

 !Sum of tan value for first and last (a and b) stored in area (variable)
 area= tan(a)+tan(b_radians)
 write (6,*) 'Initial area (sum at x(0) and x(12))', area

 !Calculate points inbetween
 !Adding 2xtan(pixi)/180 to move along each trapezoid to the right
 !Converting degrees to radians 
 i=5
 do while(i .lt. 60)
   area=area+2*tan((pi*i)/180.0)
   i=i+5
 end do

 !Convert to radians
 mult_radians=(pi*((b_degrees-a)/(2*N)))/180.0
 area=mult_radians*area

 !Approximated result
 write (6,*) ' Trapezoidal result is ',area
 !Real result
 !Integeral of tan is ln(2)
 write(6,*) ' Real result is ' ,log(2.0)

 stop 
end program
