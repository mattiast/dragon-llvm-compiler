{
	int i; int j; float[10][10] a;
	i = 0;
	while(true) {
		j = 0;
		while(true) {
			a[i][j] = 0.0;
			j=j+1;
			if( j >= 10 ) break;
		}
		i=i+1;
		if( i >= 10 ) break;
	}
	i = 0;
	while(true) {
		a[i][i] = 1.0;
		i=i+1;
		if( i >= 10 ) break;
	}
}
