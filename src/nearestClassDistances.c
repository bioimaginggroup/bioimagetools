/*
## Copyright (c) 2016, Volker Schmid
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

double nearestClassDistances(int* img, int* coord, int *dim, double* zscale0, int* cl){
  int x=coord[0]-1;
  int y=coord[1]-1;
  int z=coord[2]-1;
  int X=dim[0];
  int Y=dim[1];
  int Z=dim[2];
  int c=cl[0];
  int w=0;
  double dist=(double)X;
  double zscale=zscale0[0];
  if (Y>dist){dist=Y;}
  if ((zscale*Z)>dist){dist=zscale*Z;}
  if (zscale0[1]<dist){dist=zscale0[1];}
  double d;
  int wz=0;
  while((dist>=(double)(w+1))&(dist>=(double)(w+1)*zscale))
  {
    w++;
    //if (w>20){Rprintf("%i ",x);Rprintf("%i ",y);Rprintf("%i\n",z);}
    wz=floor(w*zscale);
    for (int k=(-wz);k < (wz+1); k++) 
    {  
      for (int i=(-w); i < (w+1); i++)
      {
        if(img[getid(x+i,y+w,z+k,X,Y,Z)]==c)
        {
          d=sqrt((double)(i*i)+(double)(w*w)+(double)(k*k)*zscale*zscale);
          if (d<dist){dist=d;}
        }
      }
    }
    for (int k=(-wz); k<=wz; k++)
    {  
      for (int i=(-w); i<=w;  i++)
      {
        if(img[getid(x+i,y-w,z+k,X,Y,Z)]==c)
        {
          d=sqrt((double)(i*i)+(double)(w*w)+(double)(k*k)*zscale*zscale);
          if (d<dist){dist=d;}
        }
      }
    }
    for (int k=(-wz); k<=wz; k++)
    {  
      for (int i=(-w); i<=w; i++)
      {
        if(img[getid(x+w,y+i,z+k,X,Y,Z)]==c)
        {
          d=sqrt((double)(i*i)+(double)(w*w)+(double)(k*k)*zscale*zscale);
          if (d<dist){dist=d;}
        }
      }
    }
    for (int k=(-wz); k<=wz; k++)
    {  
      for (int i=(-w); i<=w; i++)
      {
        if(img[getid(x-w,y+i,z+k,X,Y,Z)]==c)
        {
          d=sqrt((double)(i*i)+(double)(w*w)+(double)(k*k)*zscale*zscale);
          if (d<dist){dist=d;}
        }
      }
    }
    for (int j=(-w); j<=w; j++)
    {  
      for (int i=(-w); i<=w; i++)
      {
        if(img[getid(x+i,j+i,z+wz,X,Y,Z)]==c)
        {
          d=sqrt((double)(i*i)+(double)(j*j)+(double)(wz*wz)*zscale*zscale);
          if (d<dist){dist=d;}
        }
      }
    }
    for (int j=(-w); j<=w; j++)
    {  
      for (int i=(-w); i<=w; i++)
      {
        if(img[getid(x+i,j+i,z-wz,X,Y,Z)]==c)
        {
          d=sqrt((double)(i*i)+(double)(j*j)+(double)(wz*wz)*zscale*zscale);
          if (d<dist){dist=d;}
        }
      }
    }
    
  }//end while
  return(dist);
}

void nearestClassDistancesClass(double* dist, int* coords, int* coord, double* zscale, int* cl, int* n0, int* img, int* dim)
{
  int n=n0[0];
  for (int i=0; i<n; i++)
  {
    for (int j=0; j<3; j++)
    {
      coord[j]=coords[i*3+j];
    }
    dist[i] = nearestClassDistances(img, coord, dim, zscale, cl);
  }
}

