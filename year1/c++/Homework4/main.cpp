#include "LN.h"
#include "return_codes.h"

#include <fstream>
#include <stack>
#include <string>

LN operator""_ln(const char* number);

int main(int argc, char** argv)
{
	if (argc != 3)
	{
		fprintf(stderr, "Wrong number of command line arguments! You should write: <input_file_name> <output_file_name>\n");
		return ERROR_INVALID_PARAMETER;
	}
	std::ifstream in;
	in.open(argv[1], std::ios_base::in);
	if (!in.is_open())
	{
		fprintf(stderr, "Cannot find the input file: %s\n", argv[1]);
		return ERROR_FILE_NOT_FOUND;
	}

	std::stack< LN > st;
	try
	{
		while (!in.eof())
		{
			std::string str;
			std::getline(in, str);
			if (str == "+")
			{
				LN second = st.top();
				st.pop();
				LN first = st.top();
				st.pop();
				LN result = first + second;
				st.push(result);
			}
			else if (str == "-")
			{
				LN second = st.top();
				st.pop();
				LN first = st.top();
				st.pop();
				LN result = first - second;
				st.push(result);
			}
			else if (str == "*")
			{
				LN second = st.top();
				st.pop();
				LN first = st.top();
				st.pop();
				LN result = first * second;
				st.push(result);
			}
			else if (str == "/")
			{
				LN second = st.top();
				st.pop();
				LN first = st.top();
				st.pop();
				LN result = first / second;
				st.push(result);
			}
			else if (str == "%")
			{
				LN second = st.top();
				st.pop();
				LN first = st.top();
				st.pop();
				LN result = first % second;
				st.push(result);
			}
			else if (str == "~")
			{
				LN result = ~st.top();
				st.pop();
				st.push(result);
			}
			else if (str == "_")
			{
				LN result = -st.top();
				st.pop();
				st.push(result);
			}
			else if (str == "<")
			{
				LN second = st.top();
				st.pop();
				LN first = st.top();
				st.pop();
				LN result = first < second ? 1ll : 0ll;
				st.push(result);
			}
			else if (str == "<=")
			{
				LN second = st.top();
				st.pop();
				LN first = st.top();
				st.pop();
				LN result = first <= second ? 1ll : 0ll;
				st.push(result);
			}
			else if (str == ">")
			{
				LN second = st.top();
				st.pop();
				LN first = st.top();
				st.pop();
				LN result = first > second ? 1ll : 0ll;
				st.push(result);
			}
			else if (str == ">=")
			{
				LN second = st.top();
				st.pop();
				LN first = st.top();
				st.pop();
				LN result = first >= second ? 1ll : 0ll;
				st.push(result);
			}
			else if (str == "==")
			{
				LN second = st.top();
				st.pop();
				LN first = st.top();
				st.pop();
				LN result = first == second ? 1ll : 0ll;
				st.push(result);
			}
			else if (str == "!=")
			{
				LN second = st.top();
				st.pop();
				LN first = st.top();
				st.pop();
				LN result = first != second ? 1ll : 0ll;
				st.push(result);
			}
			else if (str.empty())
				continue;
			else
			{
				std::string_view str_view{ str };
				LN number(str_view);
				st.push(number);
			}
		}
	} catch (const char* message)
	{
		fprintf(stderr, "%s", message);
		in.close();
		return ERROR_OUTOFMEMORY;
	}
	in.close();

	FILE* out = fopen(argv[2], "w");
	if (!out)
	{
		fprintf(stderr, "Cannot find the output file: %s\n", argv[2]);
		return ERROR_FILE_NOT_FOUND;
	}
	while (!st.empty())
	{
		st.top().print(out);
		st.pop();
		fprintf(out, "\n");
	}
	fclose(out);
	return 0;
}