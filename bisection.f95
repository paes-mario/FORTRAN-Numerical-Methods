!Root finding by Bisection Method
Program Bisection

    ! defining variables types
    real :: a,b,tol,m

    ! bisection interval [a,b]
    a=1.0
    b=2.0

    ! tolerance
    tol=0.000001

    ! bisection method
    m=(a+b)/2.0
    
    ! the function
    f = exp(-m*m)-cos(m)

    ! iterative bisection method loop
    do while (abs(f)>tol)

        f = exp(-m*m)-cos(m)
        g = exp(-a*a)-cos(a)

        if (f*g<0.0) then
            b=m
        else
            a=m
        endif

        m=(a+b)/2.0
    end do
    
    ! displaying result
    print*, "A root of f(x)=exp(-x^2)-cos(x) is: ", m
    
end Program Bisection

!Output:
!A root of f(x)=exp(-x^2)-cos(x) is: 1.44741249
