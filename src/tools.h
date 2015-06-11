#ifndef EBIMAGE_TOOLS_H
#define EBIMAGE_TOOLS_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MODE_GRAYSCALE  0
#define MODE_COLOR      2
#define MODE_MAX        2

typedef struct {
    int x, y;
} PointXY;

typedef struct {
  int x, y, z;
} PointXYZ;
  
typedef double numeric;

double distanceXY (const PointXY, const PointXY);
double distancexy (int, int, int, int);
PointXY pointFromIndex (const int, const int);
int indexFromPoint (const PointXY, const int);
int indexFromXY (const int, const int, const int);
int getColorMode (SEXP x);
int validImage (SEXP x,int test);
int getNumberOfFrames (SEXP x, int type);
int getNumberOfChannels (SEXP x);
void getColorStrides(SEXP x,int index,int *redstride,int *greenstride,int *bluestride);

#ifdef __cplusplus
};
#endif

#endif
