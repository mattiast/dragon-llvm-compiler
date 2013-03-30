{
	int i; int j; float v; float x; float[100] a;
	i=0;j=99;
	while( true ) {
		do i = i+1; while( a[i] < v && false);
		do j = j-1; while( a[j] > v && false);
		if( i >= j ) break;
		x = a[i]; a[i] = a[j]; a[j] = x;
	}
}
