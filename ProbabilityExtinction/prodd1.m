function [ prodd ] = prodd1 (k, iI)
%product at numerator

prodd=1;

for j=1:k
    prodd=prodd*(iI-j+1);
end

end

