function [prob] = probscript(alpha,beta,degree)
N=10000;
%alpha, beta, nu e R from manuscript
alph = alpha;
bett = beta;
nnu=1;
R=bett/nnu;

kmax=floor(N^(1/(alph-1)));
%degree of the node from which the epidemic starts
%i should be < kmax
grado=degree;
if grado > kmax
    disp('error')
end
%normalization of probabilities
probb=zeros(kmax,1);
sommapro=0;
sommabia=0;  %need to define q

for j=1:kmax  
    probb(j)=j^(-alph);
    sommapro=sommapro+probb(j);
    sommabia=sommabia+j*probb(j);
end

probb=probb/sommapro;

%creation of pobabilities q
qrob=zeros(kmax-1,1);

for j=1:kmax 
    qrob(j)=j*probb(j)/sommabia;
end

%sum(qrob)

vecs=zeros(kmax,1); %starting vector to find s
vecsuno=vecs; %temp 
mat = zeros(kmax+1,kmax);
myvec = (0:kmax)';
 myvec = log(1+R*myvec);
 parsums = cumsum(myvec);
for i=1:kmax
    for k=0:i
        provv = gammaln(i+1)-gammaln(i+1-k)-parsums(i+1);
        if i > k
            provv = provv + parsums(i-k);
        end
        provv = provv + k*log(R);
         mat(k+1,i) = exp(provv);
    end
end

ITER=100;  %number of iterations
accuracy = 5e-5;
for p=1:ITER
    term = dot(qrob,vecs);
    for i=1:kmax
        essei=0;

        for k=0:i
            essei=essei+(term)^k*mat(k+1,i);
        end
        vecsuno(i)=essei;   %function f of manuscript; the subscript is i
    end
    if (max(abs(vecsuno-vecs))< accuracy) 
        fprintf("Aborted at iteration %d \n ",p);
        break
    end
    vecs=vecsuno;
end

prob = vecs(degree,1);    %probability of extinction
end
