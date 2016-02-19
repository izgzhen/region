fn main() {
    let x0 =  |x | {
        x
    };
    let x1 : i32=  1;
    let x2 =  x0(x1);
    drop(x1);
    drop(x0);
}