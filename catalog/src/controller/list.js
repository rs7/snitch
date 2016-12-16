const href_slice_length = [
    22,
    23 + selection1E6.toString().length,
    24 + selection1E6.toString().length + selection1E4.toString().length,
    2
][selection.length];

//+href.slice(href_slice_length) + 1;
//+href.slice(href_slice_length) + selection1E6 * 100 + 1;
//+href.slice(href_slice_length) + selection1E6 * 10000 + selection1E4 * 100 + 1;
//+href.slice(href_slice_length);
