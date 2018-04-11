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

int getid(int x,int y,int z,int X,int Y,int Z);

void varfilter(double* intensity, double* filtered,
	 double* settings, int* dims, int* filteredint, double* minmax, int* silent0) 
{
GetRNGstate();
int silent=silent0[0];
int X=dims[0];
int Y=dims[1];
int Z=dims[2];
int N=X*Y*Z;
double range=settings[0];
double zrange=settings[1];
int id,nid;
double sum,sum2,intens,n;
minmax[1]=0.0;
minmax[0]=100000000000.0;
int counter=0;
int counter2=10;
if(silent==0){Rprintf("00");}
for (int z=0; z<Z; z++)
    {
for (int x=0; x<X; x++)
{
  for (int y=0; y<Y; y++)
   {
	counter++;
	if(silent==0)
  {
    if (counter>0.1*N){
	Rprintf("\b\b\b%i",counter2);
	counter2=counter2+10;
	counter=0;
	}
  }

    	id=getid(x,y,z,X,Y,Z);  
        sum=0.0;
        sum2=0.0; 
        n=0.0;
	for (int i=ceil(-range); i<range; i++)
	{
	for (int j=ceil(-range); j<range; j++)
	{
	for (int k=ceil(-range*zrange); k<(zrange*range); k++)
	{
	if (((x+i)>=0)&((y+j)>=0)&((z+k)>=0)&(i<(X-x))&(j<(Y-y))&(k<(Z-z)))
	{
	if (sqrt(i*i+j*j+k*k/zrange/zrange)<range)
	{
	nid=getid(x+i,y+j,z+k,X,Y,Z);   
	intens=intensity[nid];   
	sum+=intens;
	sum2+=intens*intens;
	n++;
	}}}}}
        filtered[id]=sum2/n-sum*sum/n/n;
        if (filtered[id]>minmax[1]){minmax[1]=filtered[id];}
        if (filtered[id]<minmax[0]){minmax[0]=filtered[id];}
    }
  }
}
if(silent==0){Rprintf("\b\b\b\b");}

for (id=0; id<(X*Y*Z); id++)
{
filteredint[id]=floor((filtered[id]-minmax[0])/(minmax[1]-minmax[0])*65535);
}
  if(silent==0)
  {
Rprintf("done.\n");
}
return;
}


void maxfilter(double* intensity, double* filtered,
	 double* settings, int* dims, int* filteredint, double* minmax, int* silent0) 
{
GetRNGstate();
int silent=silent0[0];
int X=dims[0];
int Y=dims[1];
int Z=dims[2];
int N=X*Y*Z;
double range=settings[0];
double zrange=settings[1];
int id,nid;
double intens,max;
minmax[1]=0.0;
minmax[0]=100000000000.0;
int counter=0;
int counter2=10;
if(silent==0){Rprintf("00");}
for (int z=0; z<Z; z++)
    {
for (int x=0; x<X; x++)
{
    for (int y=0; y<Y; y++)
   {
	counter++;
  if(silent==0)
  {
	if (counter>0.1*N){
	Rprintf("\b\b\b%i",counter2);
	counter2=counter2+10;
	counter=0;
	}
}
    	id=getid(x,y,z,X,Y,Z);  
        max=0.0; 
	for (int i=ceil(-range); i<range; i++)
	{
	for (int j=ceil(-range); j<range; j++)
	{
	for (int k=ceil(-range*zrange); k<(zrange*range); k++)
	{
	if (((x+i)>=0)&((y+j)>=0)&((z+k)>=0)&(i<(X-x))&(j<(Y-y))&(k<(Z-z)))
	{
	if (sqrt(i*i+j*j+k*k/zrange/zrange)<range)
	{
	nid=getid(x+i,y+j,z+k,X,Y,Z);   
	intens=intensity[nid];   
	if(intens>max){max=intens;}
	}}}}}
        filtered[id]=max;
        if (filtered[id]>minmax[1]){minmax[1]=filtered[id];}
        if (filtered[id]<minmax[0]){minmax[0]=filtered[id];}
    }
  }
}
  if(silent==0)
  {
Rprintf("\b\b\b\b");
}
for (id=0; id<(X*Y*Z); id++)
{
filteredint[id]=floor((filtered[id]-minmax[0])/(minmax[1]-minmax[0])*65535);
}
  if(silent==0)
  {
Rprintf("done.\n");
}
return;
}

void minfilter(double* intensity, double* filtered,
	 double* settings, int* dims, int* filteredint, double* minmax, int* silent0) 
{
GetRNGstate();
int silent=silent0[0];
int X=dims[0];
int Y=dims[1];
int Z=dims[2];
int N=X*Y*Z;
double range=settings[0];
double zrange=settings[1];
int id,nid;
double max,intens;
minmax[1]=0.0;
minmax[0]=100000000000.0;
int counter=0;
int counter2=10;
  if(silent==0)
  {
Rprintf("00");
}
for (int z=0; z<Z; z++)
    {
for (int x=0; x<X; x++)
{
  for (int y=0; y<Y; y++)
   {
	counter++;
  if(silent==0)
  {
	if (counter>0.1*N){
	Rprintf("\b\b\b%i",counter2);
	counter2=counter2+10;
	counter=0;
	}
}

    	id=getid(x,y,z,X,Y,Z);  
        max=19999999.0; 
	for (int i=ceil(-range); i<range; i++)
	{
	for (int j=ceil(-range); j<range; j++)
	{
	for (int k=ceil(-range*zrange); k<(zrange*range); k++)
	{
	if (((x+i)>=0)&((y+j)>=0)&((z+k)>=0)&(i<(X-x))&(j<(Y-y))&(k<(Z-z)))
	{
	if (sqrt(i*i+j*j+k*k/zrange/zrange)<range)
	{
	nid=getid(x+i,y+j,z+k,X,Y,Z);   
	intens=intensity[nid];   
	if(intens<max){max=intens;}
	}}}}}
        filtered[id]=max;
        if (filtered[id]>minmax[1]){minmax[1]=filtered[id];}
        if (filtered[id]<minmax[0]){minmax[0]=filtered[id];}
    }
  }
}
  if(silent==0)
  {
Rprintf("\b\b\b\b");
}
for (id=0; id<(X*Y*Z); id++)
{
filteredint[id]=floor((filtered[id]-minmax[0])/(minmax[1]-minmax[0])*65535);
}
  if(silent==0)
  {
Rprintf("done.\n");
}
return;
}

















