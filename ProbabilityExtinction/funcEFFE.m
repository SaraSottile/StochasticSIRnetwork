function [ essei ] = funcEFFE (qrob, iI, vecs, R)

essei=0;

for k=0:iI
    essei=essei+(R*dot(qrob,vecs))^k*prodd1(k,iI)/prodd2(k,iI,R);
end