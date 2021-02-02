#[allow(dead_code)]
mod matrix;

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

}
