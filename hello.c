int global0 = 0;
int gloabl2 = 0;

struct st1 { int field1; long field2; };

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

void func3(void)
{
	func2(foo, 2);
}
