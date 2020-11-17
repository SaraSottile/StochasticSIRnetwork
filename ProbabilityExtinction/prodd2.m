function [ prodd ] = prodd2 (k, iI, R)
%product at denominator

prodd=1;

for j=0:k
    prodd=prodd*(R*(iI-j)+1);
end

end
