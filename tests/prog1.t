{
	int r; int dd; int a; int d;
	d=19;
	a=1234523;
	r = a; dd = d;
	while( dd <= r ) dd = 2*dd;
	while( dd != r ) {
		dd = dd/2;
		if (dd <= r) r = r - dd;
	}
}
