
format long
M = csvread('table_initialstate.csv',1,0);
N = 100;
prob = zeros(N,1);

for i = 1:100
    i
    alpha = M(i,2);
    beta = M(i,1);
    degree = M(i,4);
    prob(i) = probscript(alpha,beta,degree);
end

prob