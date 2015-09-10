%%%%%%%%%%Problem 2(c)%%%%%%%%%%
p = [63 0 -70 0 15 0];
true = sort(roots(p))
[wts,nodes] = legendreroot(5);
approx = sort(nodes)