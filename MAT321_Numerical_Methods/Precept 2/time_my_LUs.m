n = 500;
A = rand(n,n);

t=tic; [L,U] = LU_3loops(A); time_3loops = toc(t)
t=tic; [L,U] = LU_2loops(A); time_2loops = toc(t)
t=tic; [L,U] = LU_1loop(A); time_1loop_repmat = toc(t)
t=tic; [L,U] = LU_1loop_rank1(A); time_1loop_rank1 = toc(t)

