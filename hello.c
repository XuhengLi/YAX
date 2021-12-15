int global0 = 0;
int gloabl2 = 0;

struct st1 { int field1; long field2; };

/*
int func1(char arg1, unsigned long arg2)
{
	int i = 0;
	struct foo FOO;

	global2 = 4;
	printf("hello world\n");

	return i;
}

int func2(char arg3, unsigned long arg4)
{
	int i = 0;
	struct foo FOO;

	global1 = 5;
	printf("hello world\n");

	return global1;
}
*/

void func3(int arg5)
{
	int j, k, i;

	j = global2;

	j = cond1 ? if_true : else_false;
	j = cond2 ? : only_false;
	k = st1.fieldk;
	i = st2->fieldi;
	m = st3()->fieldm;
	if (cond1)
		j = global1;
	else
		j = beef;
	func2(foo, 2, bar, arg5, dead);
}
