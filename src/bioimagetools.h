//filter.c
void varfilter(double* intensity, double* filtered, double* settings, int* dims, int* filteredint, double* minmax, int* silent0);
void maxfilter(double* intensity, double* filtered,
	 double* settings, int* dims, int* filteredint, double* minmax, int* silent0); 
void minfilter(double* intensity, double* filtered,
	 double* settings, int* dims, int* filteredint, double* minmax, int* silent0);

//nearestClassDistances.c
void nearestClassDistancesClass(double* dist, int* coords, int* coord, double* zscale, int* cl, int* n0, int* img, int* dim);

//segment.c
void segment_cem(double* intensity, 
	 int* class, int* mask,
	 double* mu, double* sigma,
	 int* dims, int* settings, double* loglik,
	double* beta, double* betaz);
void segment_cem2d(double* intensity, 
	 int* class, int* mask,
	 double* mu, double* sigma,
	 int* dims, int* settings, double* loglik,
	double* beta, double* betaz);
void segment_em(double* intensity, 
	 double* p, int* mask, 
         int* class,
	 int* dims, int* i0, 
	double* beta0, double* betaz0);

