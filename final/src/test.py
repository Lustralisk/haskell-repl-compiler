def main():
	a = 1
	def f():
		def g():
			print(a)
		g()
	def h():
		a = 2
		f()
	h()


def foo(n):
	def bar(n):
		return n * foo(n-1)
	if n > 0:
		return bar(n)
	else:
		return 1

main()