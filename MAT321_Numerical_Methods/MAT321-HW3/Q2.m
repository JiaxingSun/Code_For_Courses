tm = rand(5,5);
tma = tm*tm';%create the random symmetric matrix
[egvector,egvalue,nmseries] = jacobi1(tma, 1e-14);%use my own function to calculate the eigen pairs
plot(nmseries,'-o');%plot the norm of off-diag elements
xlabel('No. of iterates');
ylabel('sum of square of off-diag elements');

[d,ind] = sort(diag(egvalue));
egvector = egvector(:,ind)
egvalue = diag(d)

[egve,egva] = eig(tma)%use the Matlab function eig
ang = subspace(egve,egvector)%compare the two subspaces spanned by eigenvectors

