clear all; close all; clc;
hseries = logspace(-16,-1);
x0 = pi/4;
func = @(x)exp(x)./(cos(x).^3+sin(x).^3);
dfunc = @(x)(exp(x).*(cos(x).^3+sin(x).^3)-exp(x).*(-3*cos(x).^2.*sin(x)+3*sin(x).^2.*cos(x)))./(cos(x).^3+sin(x).^3).^2;
numdif = imag(func(x0+i*hseries)./hseries);
realdif = dfunc(x0);
err = abs(numdif-realdif);
loglog(hseries, err, 'bo');
hold on;
loglog(hseries, 20*hseries.^2, 'k-');
ylabel('absolute error');
xlabel('h');
[min_err, min_i] = min(err);
min_h = hseries(min_i)
min_err