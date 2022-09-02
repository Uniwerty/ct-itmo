#include <omp.h>
#include <iostream>
#include <algorithm>
#include <vector>
#include <string>
#include <fstream>

using namespace std;

struct pixel {
    int r;
    int g;
    int b;
};

int thread_num;
string input, output;
float factor;
unsigned char magic0, magic1;
int width, height, maxval;
vector<pixel> image;
vector<int> red, green, blue;
vector<int> gray_image;

int main(int argc, char* argv[]) {
    try {
        thread_num = stoi(argv[1]);
        input = argv[2];
        output = argv[3];
        factor = stof(argv[4]);
    
        ifstream in;
        in.open(input, ios::binary);
        if (!in.is_open()) {
            throw "Cannot open file: " + input;
        }
        ofstream out;
        out.open(output, ios::binary);
        if (!out.is_open()) {
            throw "Cannot open file: " + output;
        }
        // Reading identification chars
        magic0 = in.get();
        magic1 = in.get();
        out.put(magic0);
        out.put(magic1);
        out.put(in.get());
        if (magic0 != 'P' || magic1 != '5' && magic1 != '6') {
            throw "Incorrect file format";
        }
        // Reading image width
        unsigned char c = in.get();
        while (!isspace(c)) {
            out.put(c);
            width = width * 10 + (c - '0');
            c = in.get();
        }
        out.put(c);
        // Reading image height
        c = in.get();
        while (!isspace(c)) {
            out.put(c);
            height = height * 10 + (c - '0');
            c = in.get();
        }
        out.put(c);
        // Reading color maxvalue
        c = in.get();
        while (!isspace(c)) {
            out.put(c);
            maxval = maxval * 10 + (c - '0');
            c = in.get();
        }
        out.put(c);
        // Reading image
        if (magic1 == '5') {
            // Grayscale
            gray_image.resize(width * height);
            for (int i = 0; i < width * height; i++) {
                gray_image[i] = in.get();
            }
        }
        else if (magic1 == '6') {
            // RGB
            image.resize(width * height);
            red.resize(width * height);
            green.resize(width * height);
            blue.resize(width * height);
            for (int i = 0; i < width * height; i++) {
                image[i].r = in.get();
                image[i].g = in.get();
                image[i].b = in.get();
                red[i] = image[i].r;
                green[i] = image[i].g;
                blue[i] = image[i].b;
            }
        }
        double start = omp_get_wtime();
        // Finding minimal and maximal values according to factor
        int min_value = 0;
        int max_value = 0;
        int index = (int)(width * height * factor) - 1;
        if (magic1 == '5') {
            // Grayscale
            vector<int> values = gray_image;
            nth_element(values.begin(), values.begin() + index, values.end());
            min_value = values[index];
            nth_element(values.begin(), values.end() - index - 1, values.end());
            max_value = values[values.size() - index - 1];
        }
        else if (magic1 == '6') {
            // RGB
            int min_red, min_green, min_blue;
            int max_red, max_green, max_blue;
            nth_element(red.begin(), red.begin() + index, red.end());
            min_red = red[index];
            nth_element(green.begin(), green.begin() + index, green.end());
            min_green = green[index];
            nth_element(blue.begin(), blue.begin() + index, blue.end());
            min_blue = blue[index];

            min_value = min(min_red, min(min_green, min_blue));
            nth_element(red.begin(), red.end() - index - 1, red.end());
            max_red = red[red.size() - index - 1];
            nth_element(green.begin(), green.end() - index - 1, green.end());
            max_green = green[green.size() - index - 1];
            nth_element(blue.begin(), blue.end() - index - 1, blue.end());
            max_blue = blue[blue.size() - index - 1];
            max_value = max(max_red, max(max_green, max_blue));
        }
        // Calculating new values
        if (magic1 == '5') {
            // Grayscale
            #pragma omp parallel num_threads(thread_num)
            {
                #pragma omp for schedule(runtime)
                for (int i = 0; i < width * height; i++) {
                    gray_image[i] = (gray_image[i] - min_value) * 255 / (max_value - min_value);
                    if (gray_image[i] < 0) gray_image[i] = 0;
                    else if (gray_image[i] > 255) gray_image[i] = 255;
                }
            }
        }
        else if (magic1 == '6') {
            // RGB
            #pragma omp parallel num_threads(thread_num)
            {
                #pragma omp for schedule(runtime)
                for (int i = 0; i < width * height; i++) {
                    image[i].r = (image[i].r - min_value) * 255 / (max_value - min_value);
                    image[i].g = (image[i].g - min_value) * 255 / (max_value - min_value);
                    image[i].b = (image[i].b - min_value) * 255 / (max_value - min_value);
                    if (image[i].r < 0) image[i].r = 0;
                    if (image[i].g < 0) image[i].g = 0;
                    if (image[i].b < 0) image[i].b = 0;
                    if (image[i].r > 255) image[i].r = 255;
                    if (image[i].g > 255) image[i].g = 255;
                    if (image[i].b > 255) image[i].b = 255;
                }
            }
        }
        double end = omp_get_wtime();
        // Writing new image
        if (magic1 == '5') {
            for (int i = 0; i < width * height; i++) {
                out.put((char)gray_image[i]);
            }
        }
        else if (magic1 == '6') {
            for (int i = 0; i < width * height; i++) {
                out.put((char)image[i].r);
                out.put((char)image[i].g);
                out.put((char)image[i].b);
            }
        }
        in.close();
        out.close();
        printf("Time (%i thread(s)): %g ms \n", thread_num, (end - start) * 1000);
    }
    catch (string e) {
        cerr << e << '\n';
    }
    return 0;
}