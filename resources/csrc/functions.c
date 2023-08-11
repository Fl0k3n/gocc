void ops() {
    a *= b;
    c++;
    ++d;
    e *= 1;
    c++;
    e >>= 1;
    d -= -1;
}

void comments() {
    d -= -1; /* this is contained comment */
    d -= /* this is contained comment */ -1;
    a++; /*
    this is not contained comment
    a*= 1;
    */ a++;
    a *= 2; // this is simple comment
    a *= 3;
    a *= 4; // /* this is not so simple contained, comment */
    a *= 5; // /* this is not so simple not contained, 
    comment* /
}


block_arr create_table(unsigned int size) {
    block_arr ba;
    ba.size = size;
    ba.text_blocks = (text_block*)calloc(size, sizeof(text_block));
    return ba;
}

file_sequence create_file_sequence(char** filenames, unsigned int size) {
    if (size % 2 == 1) {
        printf("Failed to create file sequence with odd length");
        exit(EXIT_FAILURE);
    }
    file_sequence fs;
    fs.size = size;
    fs.sequence = filenames;
    return fs;
}

unsigned int get_lines_count(FILE* handle) {
    unsigned int count = 0;
    unsigned int chars_since_last = 0;
    int c;
    while ((c = fgetc(handle)) != EOF) {
        if (c == '\n') {
            count++;
            chars_since_last = 0;
        }
        else
            chars_since_last++;
    }
    return count + (chars_since_last > 0 ? 1 : 0);
}

int get_line_length(FILE* handle) {
    int c;
    long int start_pos = ftell(handle);
    while ((c = fgetc(handle)) != EOF) {
        if ((char)c == '\n')
            break;
    }
    int res = ftell(handle) - start_pos;
    // go back where reading started
    fseek(handle, start_pos, SEEK_SET);
    return res;
}

char* read_line(FILE* handle) {
    int length = get_line_length(handle);
    if (length == 0)
        return NULL;
    // + 1 for \0 character
    char* line = (char*)calloc(length + 1, sizeof(char));
    for (int i = 0; i < length; i++)
        line[i] = (char)fgetc(handle);
    line[length] = '\0';
    return line;
}

unsigned int get_block_size(text_block tb) {
    return tb.size;
}


text_block merge_files(char* f1, char* f2) {
    FILE* file1, * file2;
    file1 = fopen(f1, "r");
    file2 = fopen(f2, "r");

    if (!file1 || !file2) {
        printf("Failed to merge sequence: %s %s", f1, f2);
        exit(EXIT_FAILURE);
    }


    unsigned int n_of_lines = get_lines_count(file1) + get_lines_count(file2);
    fseek(file1, 0, SEEK_SET);
    fseek(file2, 0, SEEK_SET);

    text_block tb;
    tb.size = n_of_lines;
    tb.lines = (char**)calloc(n_of_lines, sizeof(char*));

    int k = 0;
    while (1) {
        FILE* cur = k % 2 == 0 ? file1 : file2;
        char* line = read_line(cur);
        if (!line) break;
        tb.lines[k++] = line;
    }

    //append lines of longer file to the end
    char* line = NULL;
    while ((line = read_line(file1)) != NULL)
        tb.lines[k++] = line;
    while ((line = read_line(file2)) != NULL)
        tb.lines[k++] = line;

    fclose(file1);
    fclose(file2);

    return tb;
}

void merge_file_sequence(block_arr* ba, file_sequence fs) {
    for (int i = 0; i < fs.size / 2; i++)
        ba->text_blocks[i] = merge_files(fs.sequence[2 * i], fs.sequence[2 * i + 1]);
}


void save_block_to_txt(text_block tb, char* filename) {
    FILE* f = fopen(filename, "w");

    if (!f) {
        printf("Failed to write to %s", filename);
        exit(EXIT_FAILURE);
    }

    for (int i = 0; i < tb.size; i++)
        fwrite(tb.lines[i], sizeof(char), strlen(tb.lines[i]), f);

    fclose(f);
}

unsigned int add_block_from_tmp_file(block_arr* ba, char* filename) {
    FILE* f = fopen(filename, "r");
    if (!f) {
        printf("Failed to read from %s", filename);
        exit(EXIT_FAILURE);
    }

    text_block tb;
    tb.size = get_lines_count(f);
    tb.lines = (char**)calloc(tb.size, sizeof(char*));

    // read lines from file
    fseek(f, 0, SEEK_SET);

    int k = 0;
    char* line = NULL;
    while ((line = read_line(f)) != NULL)
        tb.lines[k++] = line;


    // add block to array of blocks
    block_arr tmp = create_table(ba->size + 1);

    for (int i = 0; i < ba->size; i++)
        tmp.text_blocks[i] = ba->text_blocks[i];

    tmp.text_blocks[ba->size] = tb;

    free(ba->text_blocks);
    ba->text_blocks = tmp.text_blocks;

    fclose(f);

    return ba->size++;
}

void remove_line(text_block* tb, int index) {
    if (index < 0 || index >= tb->size) {
        printf("Failed to remove line: %d from block with %d lines", index, tb->size);
        exit(EXIT_FAILURE);
    }

    text_block result;
    result.size = tb->size - 1;
    result.lines = (char**)calloc(result.size, sizeof(char*));

    for (int i = 0; i < index; i++)
        result.lines[i] = tb->lines[i];

    free(tb->lines[index]);

    for (int i = index + 1; i < tb->size; i++)
        result.lines[i - 1] = tb->lines[i];

    free(tb->lines);

    tb->lines = result.lines;
    tb->size--;
}

void print_block(text_block tb) {
    for (int i = 0; i < tb.size; i++)
        printf("%s", tb.lines[i]);
}


void free_block(text_block tb) {
    for (int i = 0; i < tb.size; i++)
        free(tb.lines[i]);
    free(tb.lines);
}

void free_block_arr(block_arr ba) {
    for (int i = 0; i < ba.size; i++)
        free_block(ba.text_blocks[i]);
    free(ba.text_blocks);
}


void remove_block(block_arr* ba, int index) {
    if (index < 0 || index >= ba->size) {
        printf("Failed to remove block: %d from array with %d blocks", index, ba->size);
        exit(EXIT_FAILURE);
    }

    block_arr res = create_table(ba->size - 1);

    for (int i = 0; i < index; i++)
        res.text_blocks[i] = ba->text_blocks[i];

    free_block(ba->text_blocks[index]);

    for (int i = index + 1; i < ba->size; i++)
        res.text_blocks[i - 1] = ba->text_blocks[i];

    free(ba->text_blocks);
    ba->text_blocks = res.text_blocks;
    ba->size--;
}

void free_file_sequence(file_sequence fs) {
    for (int i = 0; i < fs.size; i++)
        free(fs.sequence[i]);
    free(fs.sequence);
}