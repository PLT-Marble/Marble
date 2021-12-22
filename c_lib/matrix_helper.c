/*
 *  matrix_helper.c
 *  reference: https://github.com/emilydringel/MATRIX_MANIA/blob/main/c_functions/matrix_functions.c
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void printmf(double* element_one) /* prints the matrix<int> */
{
    int rows = (int) element_one[0];
    int cols = (int) element_one[1];
    int size = rows * cols;
    char matrix[size*100]; /* maybe should be better about size?? */
    strcpy(matrix, "");
    for(int i = 0; i < rows; i++){
        for(int j = 0; j < cols; j++){
            int location = 2 + (i*cols) + j;
            char buffer[1000];
            sprintf(buffer, "%f", element_one[location]);
            strcat(matrix, buffer);
            if(j!=cols-1){
                strcat(matrix, " ");
            }
        }
        if(i!=rows-1){
            strcat(matrix, "\n");
        }
    }
    printf("%s\n", matrix);
}

double* addmf(double* m1, double* m2) 
{
    if(m1[0]!=m2[0] || m1[1]!=m2[1]){
        printf("RUNTIME ERROR: matrices being added do not have the same dimensions.\n");
        exit(1);
    }
    int rows = (int) m1[0];
    int cols = (int) m1[1];
    int size = 2 + rows * cols;
    double *empty = malloc(size * sizeof(double));
    empty[0] = rows;
    empty[1] = cols;
    for (int i = 2; i < size; i++) {
        empty[i] = m1[i] + m2[i];
    }
    return empty;
}

double* submf(double* m1, double* m2) 
{
    if(m1[0]!=m2[0] || m1[1]!=m2[1]){
        printf("RUNTIME ERROR: matrices being subtracted do not have the same dimensions.\n");
        exit(1);
    }
    int rows = (int) m1[0];
    int cols = (int) m1[1];
    int size = 2 + rows * cols;
    double *empty = malloc(size * sizeof(double));
    empty[0] = rows;
    empty[1] = cols;
    for (int i = 2; i < size; i++) {
        empty[i] = m1[i] - m2[i];
    }
    return empty;
}

double* scalarmf(double x, double* m){
    int rows = (int) m[0];
    int cols = (int) m[1];
    int size = 2 + rows * cols;
    double *empty = malloc(size * sizeof(double));
    for (int i = 0; i < size; i++) {
        empty[i] = m[i];
        if(i>=2){
            empty[i] *= x;
        }
    }
    return empty;
}

double* multiplicationf(double* m1, double* m2){
    if(m1[1]!=m2[0]){
        printf("RUNTIME ERROR: matrices being multiplied do not have complementary dimensions.\n");
        exit(1);
    }
    int rows_one = (int) m1[0];
    int rows_two = (int) m2[0];
    int cols_one = (int) m1[1];
    int cols_two = (int) m2[1];
    double *empty = malloc(rows_one * cols_two * sizeof(int));
    empty[0] = (double) rows_one;
    empty[1] = (double) cols_two;
    for(int row=0; row<rows_one; row++){
        for(int col=0; col<cols_two; col++){
            empty[2+(cols_two*row)+col] = 0;
            for(int val=0; val<cols_one; val++){
                double x = m1[2+cols_one*row+val]*m2[2+cols_two*val+col];
                empty[2+cols_two*row+col] += x;
            }
        }
    }
    return empty;
}
