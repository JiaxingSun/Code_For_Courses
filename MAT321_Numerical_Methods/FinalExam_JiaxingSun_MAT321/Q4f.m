%%%%%%%%this file is for Question 4(f)%%%%%%%%%%%%%

fhandle = @(x)(exp(-x));
lower = -2;
upper = 0.5;
totnum = 17;

err_2norm = zeros(1,(totnum+1));
for n = 3:(3+totnum)
    fh = @(x)((eval_best_2norm_simpson(fhandle,n,x,lower,upper,1000)-fhandle(x)).^2);
    err_2norm(n-2) = integral(fh,lower,upper,'AbsTol',1e-12,'ArrayValued',true);
end

for n = 3:(3+totnum)
    fh2 = @(x)(abs(eval_best_2norm_simpson(fhandle,n,x,lower,upper,1000)-fhandle(x)));
    err_supnorm(n-2) = max(fh2(linspace(lower,upper,12345)));
end
plot(3:3+totnum,err_2norm);
hold on;
plot(3:3+totnum,err_supnorm,'r');
hold off;
legend('2-norm','sup-norm');
xlabel('n');
ylabel('error');
title('Using Simpson for Integration');