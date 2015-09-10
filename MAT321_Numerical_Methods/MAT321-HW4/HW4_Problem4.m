%% 4(b) of HW4
fone = @(x)(exp(-x*x));
[V_approximate,No_intervals,E_estimated,Matrix] = myromberg(fone,-1,1,1e-9,20);
exact = erf(1)*sqrt(pi) %exact value of the integral
V_approximate %approximate integral
E_estimated %estimated error
No_intervals %number of intervals used
No_evaluations = No_intervals+1 %number of evaluations used
Matrix %the Romberg matrix

%% 4(c) of HW4
ftwo = @(x)(sin(abs(x+pi/4)));
[V_approximate,No_intervals,E_estimated,Matrix] = myromberg(ftwo,-2,0,1e-9,20);
exact = -0.5*sqrt(2)*(cos(2)+sin(2)+1)+2 %exact value of the integral
V_approximate %approximate integral
E_estimated %estimated error
No_intervals %number of intervals used
No_evaluations = No_intervals+1 %number of evaluations used
Matrix %the Romberg matrix

%% Explanation of Results
%the number of intervals used in 4(c) is much more than in 4(b), this is
%due to the incontinuity in the 1st-order derivative in the second
%function.