/*
##
##
## Copyright (c) 2011, Volker Schmid
## All rights reserved.
## 
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are
## met:
## 
##     * Redistributions of source code must retain the above copyright
##       notice, this list of conditions and the following disclaimer. 
##     * Redistributions in binary form must reproduce the above
##       copyright notice, this list of conditions and the following
##       disclaimer in the documentation and/or other materials provided
##       with the distribution.
##     * The names of the authors may not be used to endorse or promote
##       products derived from this software without specific prior
##       written permission.
## 
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
## "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
## LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
## A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
## HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
## SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
## LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
## DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
## THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
## 
##
*/

#include <R.h>
#include <Rmath.h>

int getid(int x,int y,int z,int X,int Y,int Z)
{
int id=z*X*Y+y*X+x;
return(id);
}

void idget(int id, int* xyz, int X, int Y, int Z)
{
xyz[2]=floor(id/(X*Y));
id=id-(xyz[2]*X*Y);
xyz[1]=floor(id/X);
xyz[0]=id-(xyz[1]*X);
return;
}

void segment_cem(double* intensity, 
	 int* class, int* mask,
	 double* mu, double* sigma,
	 int* dims, int* settings, double* loglik,
	double* beta, double* betaz) 
{
  GetRNGstate();

int X=dims[0];
int Y=dims[1];
int Z=dims[2];
int nclust=settings[0];
int id,nid;
int newclass;
double temp;

for (int x=0; x<X; x++)
{
  for (int y=0; y<Y; y++)
   {
    for (int z=0; z<Z; z++)
    {
      id=getid(x,y,z,X,Y,Z);  

      if (mask[id]==1)
      { 

	for (int i=0; i<nclust; i++)
	{
   	loglik[i]=-0.5*pow(intensity[id]-mu[i],2)/sigma[i]/sigma[i];
	}

	if (x!=0)
	{
	nid=getid(x-1,y,z,X,Y,Z);
	for (int i=0; i<nclust; i++)
	{
	loglik[i]+=mask[nid]*beta[getid(class[nid],i,0,nclust,nclust,1)];   	
	}
	}
	if (x!=(X-1))
	{
	nid=getid(x+1,y,z,X,Y,Z);
	for (int i=0; i<nclust; i++)
	{
	loglik[i]+=mask[nid]*beta[getid(class[nid],i,0,nclust,nclust,1)];   	
        }
	}
	if (y!=0)
	{
	nid=getid(x,y-1,z,X,Y,Z);
	for (int i=0; i<nclust; i++)
	{
	loglik[i]+=mask[nid]*beta[getid(class[nid],i,0,nclust,nclust,1)];   	
	}
	}
	if (y!=(Y-1))
	{
	nid=getid(x,y+1,z,X,Y,Z);
	for (int i=0; i<nclust; i++)
	{
	loglik[i]+=mask[nid]*beta[getid(class[nid],i,0,nclust,nclust,1)];   	
        }
	}
	if (z!=0)
	{
	nid=getid(x,y,z-1,X,Y,Z);
	for (int i=0; i<nclust; i++)
	{
	loglik[i]+=mask[nid]*betaz[getid(class[nid],i,0,nclust,nclust,1)];   	
        }
	}
	if (z!=(Z-1))
	{
	nid=getid(x,y,z+1,X,Y,Z);
	for (int i=0; i<nclust; i++)
	{
	loglik[i]+=mask[nid]*betaz[getid(class[nid],i,0,nclust,nclust,1)];   	
        }
	}

	newclass=0;
	temp=loglik[0];

	for (int i=1; i<nclust; i++)
	{
	if (loglik[i]>temp)
		{
		temp=loglik[i];
        	newclass=i;
		}
	}
	class[id]=newclass;
     }
}
}
}
return;
}
void segment_cem2d(double* intensity, 
	 int* class, int* mask,
	 double* mu, double* sigma,
	 int* dims, int* settings, double* loglik,
	double* beta, double* betaz) 
{
  GetRNGstate();

int X=dims[0];
int Y=dims[1];
int nclust=settings[0];
int id,nid;
int newclass;
double temp;
 int z=0;
 int Z=1;
for (int x=0; x<X; x++)
{
  for (int y=0; y<Y; y++)
   {
      id=getid(x,y,z,X,Y,Z);  

      if (mask[id]==1)
      { 

	for (int i=0; i<nclust; i++)
	{
   	loglik[i]=-0.5*pow(intensity[id]-mu[i],2)/sigma[i]/sigma[i];
	}

	if (x!=0)
	{
	nid=getid(x-1,y,z,X,Y,Z);
	for (int i=0; i<nclust; i++)
	{
	loglik[i]+=mask[nid]*beta[getid(class[nid],i,0,nclust,nclust,1)];   	
	}
	}
	if (x!=(X-1))
	{
	nid=getid(x+1,y,z,X,Y,Z);
	for (int i=0; i<nclust; i++)
	{
	loglik[i]+=mask[nid]*beta[getid(class[nid],i,0,nclust,nclust,1)];   	
        }
	}
	if (y!=0)
	{
	nid=getid(x,y-1,z,X,Y,Z);
	for (int i=0; i<nclust; i++)
	{
	loglik[i]+=mask[nid]*beta[getid(class[nid],i,0,nclust,nclust,1)];   	
	}
	}
	if (y!=(Y-1))
	{
	nid=getid(x,y+1,z,X,Y,Z);
	for (int i=0; i<nclust; i++)
	{
	loglik[i]+=mask[nid]*beta[getid(class[nid],i,0,nclust,nclust,1)];   	
        }
	}
	if (z!=0)
	{
	nid=getid(x,y,z-1,X,Y,Z);
	for (int i=0; i<nclust; i++)
	{
	loglik[i]+=mask[nid]*betaz[getid(class[nid],i,0,nclust,nclust,1)];   	
        }
	}
	if (z!=(Z-1))
	{
	nid=getid(x,y,z+1,X,Y,Z);
	for (int i=0; i<nclust; i++)
	{
	loglik[i]+=mask[nid]*betaz[getid(class[nid],i,0,nclust,nclust,1)];   	
        }
	}

	newclass=0;
	temp=loglik[0];

	for (int i=1; i<nclust; i++)
	{
	if (loglik[i]>temp)
		{
		temp=loglik[i];
        	newclass=i;
		}
	}
	class[id]=newclass;
     }
}
}
return;
}

void segment_em(double* intensity, 
	 double* p, int* mask, 
         int* class,
	 int* dims, int* i0, 
	double* beta0, double* betaz0) 
{
   GetRNGstate();
   int X=dims[0]; 
   int Y=dims[1]; 
   int Z=dims[2];
   int id,nid;
   int i=i0[0];
   double logp=0;
   double beta=beta0[0];
   double betaz=betaz0[0];

   for (int x=0; x<X; x++)
     {
      for (int y=0; y<Y; y++)
	 {
	   for (int z=0; z<Z; z++)
	     {
	       id=getid(x,y,z,X,Y,Z);  
	       if (mask[id]==1)
		 { 
		   logp=0;
		   if (x!=0)
		     {
		       nid=getid(x-1,y,z,X,Y,Z);
                       if (i==class[nid]){logp=logp+beta;}
                       if (i!=class[nid]){logp=logp-beta;}
                     }
		   if (x!=(X-1))
		     {
		       nid=getid(x+1,y,z,X,Y,Z);
                       if (i==class[nid]){logp=logp+beta;}
                       if (i!=class[nid]){logp=logp-beta;}
		     }
		   if (y!=0)
		     {
		       nid=getid(x,y-1,z,X,Y,Z);
                       if (i==class[nid]){logp=logp+beta;}
                       if (i!=class[nid]){logp=logp-beta;}
		     }
		   if (y!=(Y-1))
		     {
		       nid=getid(x,y+1,z,X,Y,Z);
                       if (i==class[nid]){logp=logp+beta;}
                       if (i!=class[nid]){logp=logp-beta;}
		     }
		   if (z!=0)
		     {
		       nid=getid(x,y,z-1,X,Y,Z);
                       if (i==class[nid]){logp=logp+betaz;}
                       if (i!=class[nid]){logp=logp-betaz;}
		     }
		   if (z!=(Z-1))
		     {
		       nid=getid(x,y,z+1,X,Y,Z);
                       if (i==class[nid]){logp=logp+betaz;}
                       if (i!=class[nid]){logp=logp-betaz;}
		     }
                   p[id]=p[id]*exp(logp);
		 }
	     }
	 }
     }
   return;
}

void docheck(int id, int* class, int what, int* outside, int* tocheck, int* checked, int* xyz, int blobsize, int* dims)
{
int newid;
int X=dims[0];
int Y=dims[1];
int Z=dims[2];

idget(id,xyz,X,Y,Z);

tocheck[id]=0;
checked[id]=1;

if (class[id]!=what)
{
return;
}

outside[id]=1;

for (int i=1; i<=blobsize; i++)
{
if (xyz[0]>=i)
{
newid=getid(xyz[0]-i,xyz[1],xyz[2], X, Y, Z);
if (class[newid]!=what){return;}
}
if (xyz[1]>=i)
{
newid=getid(xyz[0],xyz[1]-i,xyz[2], X, Y, Z);
if (class[newid]!=what){return;}
}
if (xyz[0]<(X-i))
{
newid=getid(xyz[0]+i,xyz[1],xyz[2], X, Y, Z);
if (class[newid]!=what){return;}
}
if (xyz[1]<(Y-i))
{
newid=getid(xyz[0],xyz[1]+i,xyz[2], X, Y, Z);
if (class[newid]!=what){return;}
}
}

if (xyz[2]>0)
{
newid=getid(xyz[0],xyz[1],xyz[2]-1, X, Y, Z);
if (class[newid]!=what){return;}
}
if (xyz[2]<(Z-1))
{
newid=getid(xyz[0],xyz[1],xyz[2]+1, X, Y, Z);
if (class[newid]!=what){return;}
}

if (blobsize>1)
{
for (int i=1; i<blobsize; i++)
{
for (int j=1; j<blobsize; j++)
{
if (xyz[0]>=i)
{
if (xyz[1]>=j)
{
newid=getid(xyz[0]-i,xyz[1]-j,xyz[2],X, Y, Z);
if (class[newid]!=what){return;}
}
}
if (xyz[0]<(X-i))
{
if (xyz[1]>=j)
{
newid=getid(xyz[0]+i,xyz[1]-j,xyz[2],X, Y, Z);
if (class[newid]!=what){return;}
}
}
if (xyz[0]>=i)
{
if (xyz[1]<(Y-j))
{
newid=getid(xyz[0]-i,xyz[1]+j,xyz[2], X, Y, Z);
if (class[newid]!=what){return;}
}
}
if (xyz[0]<(X-i))
{
if (xyz[1]<(Y-j))
{
newid=getid(xyz[0]+i,xyz[1]+j,xyz[2], X, Y, Z);
if (class[newid]!=what){return;}
}
}
}
}
}

if (xyz[0]!=0)
{
newid=getid(xyz[0]-1,xyz[1],xyz[2],X,Y,Z);
if(checked[newid]==0)tocheck[newid]=1;
}
if (xyz[0]<(X-1))
{
newid=getid(xyz[0]+1,xyz[1],xyz[2],X,Y,Z);
if(checked[newid]==0)tocheck[newid]=1;
}
if (xyz[1]!=0)
{
newid=getid(xyz[0],xyz[1]-1,xyz[2],X,Y,Z);
if(checked[newid]==0)tocheck[newid]=1;
}
if (xyz[1]<(Y-1))
{
newid=getid(xyz[0],xyz[1]+1,xyz[2],X,Y,Z);
if(checked[newid]==0)tocheck[newid]=1;
}
if (xyz[2]!=0)
{
newid=getid(xyz[0],xyz[1],xyz[2]-1,X,Y,Z);
if(checked[newid]==0)tocheck[newid]=1;
}
if (xyz[2]<(Z-1))
{
newid=getid(xyz[0],xyz[1],xyz[2]+1,X,Y,Z);
if(checked[newid]==0)tocheck[newid]=1;
}

return;
}

void outside(int* class, int* dims, int* settings, int* outside, int* tocheck, int* checked, int* temp3) 
{
  GetRNGstate();

int X=dims[0];
int Y=dims[1];
int Z=dims[2];
int N=X*Y*Z;
int what=settings[0];
int blobsize=settings[1];
int n=1;
while (n>0)
{
for (int i=0; i<N; i++)
{

if (tocheck[i]==1)
{
docheck(i,class,what,outside,tocheck,checked,temp3,blobsize,dims);
}
}

n=0;
for (int i=0; i<N; i++)
{
if (tocheck[i]==1)
{
n++;
}
}
}


return;
}










