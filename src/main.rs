#[allow(dead_code)]
mod matrix;

struct Test<T> {
    a: T
}

impl<T> Test<T> {
    pub fn allocate(&mut self, inp: T) {
        self.a = inp;
    }
}

fn main() {

    let mut m = matrix::Mat::new(3, 3);

    let var = 5.;
    m.fill(var);

    for i in 0..m.rows() {
        for j in 0..m.cols() {
            let t_i: f64 = i as f64;
            let t_j: f64 = j as f64;
            let temp: f64 = t_i*t_j;
            m[[i,j]] = temp;
        }
    }

    m.printer();

    let mut t = Test{a: 1.0};

    t.allocate(1. + 2.);

    println!("{:?}", t.a);

    let temp: Vec<f64> = vec![1.0, 2.0];

    let mut t2 = Test{a: &temp};

    let mut temp2: Vec<f64> = vec![0., 0.];

    temp2[0] = 2.;
    temp2[1] = 3.;

    t2.allocate(&temp2);

    println!("{:?}", t2.a);
    println!("{:?}", temp2);

    // add tow matrices
    let mut m1 = matrix::Mat::new(3, 3);
    let mut m2 = matrix::Mat::new(3, 3);
    m1.fill(var);
    m2.fill(var);
    for i in 0..m1.rows() {
        for j in 0..m1.cols() {
            let t_i: f64 = i as f64;
            let t_j: f64 = j as f64;
            let temp: f64 = t_i*t_j;
            m1[[i,j]] = temp + 1.;
            m2[[i,j]] = temp;
        }
    }

    //println!("{:?}", "m1");
    //m1.printer();
    let mut m1 = & m1 + & m2;
    
    //println!("{:?}", "m1");
    //m1.printer();
    //println!("{:?}", "m2");
    //m2.printer();
    //let mut m3 = &m1 + &m2;

    println!("{:?}", "m1");
    m1.printer();
    println!("{:?}", "m2");
    m2.printer();
    m2.plus(&mut m1);
    println!("{:?}", "m1");
    m1.printer();




}
