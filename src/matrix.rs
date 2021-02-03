/*
Matrix structure. Stored in vector. Column wise. 
*/

#[derive(Debug, Default)]
pub struct Mat {
    ncol: usize,
    nrow: usize,
    size: usize,
    vec: Vec<f64>
}
    
impl Mat {
    pub fn new(nrow: usize, ncol: usize) -> Mat {
        Mat {
            nrow: nrow,
            ncol: ncol,
            size: ncol*nrow,
            vec: Vec::<f64>::with_capacity(nrow*ncol)
        }
    }

    pub fn fill(&mut self, content: f64) {
        self.vec.resize(self.nrow*self.ncol, content);
    }
    
    pub fn printer(& self) {
        for i in 0..self.nrow {
            for j in 0..self.ncol {
                print!{"{:}", self.vec[i + self.nrow*j]};
                print!("{:}", " ");
            }
            println!("{:}", "\n");
        }
    }

    pub fn cols(&self) -> usize{
        return self.ncol;
    }

    pub fn rows(&self) -> usize{
        return self.nrow;
    }

    pub fn row_ret(self, _row: usize) -> Vec<f64> {
        let _temp = self.vec.clone();
        return self.vec;
    }

    pub fn col_ret(&self, _col: usize) -> Vec<f64> {
        let temp = self.vec.clone();
        return temp;
    }

    pub fn plus(& self, inp: &mut Mat) {

            for i in 0..self.nrow {
                for j in 0..self.ncol {
                    inp.vec[i + self.nrow*j] = self.vec[i + self.nrow*j] + inp.vec[i + self.nrow*j];
                }
            }
    }


}

impl std::ops::Index<[usize; 2]> for Mat {
    type Output = f64;
    fn index<'a>(&'a self, idx: [usize; 2] ) ->&'a f64{
        &self.vec[idx[1] * self.nrow + idx[0] ]
    }
}


impl std::ops::IndexMut<[usize; 2]> for Mat {
    fn index_mut<'a>(&'a mut self, idx: [usize; 2] ) ->&'a mut f64{
        &mut self.vec[idx[1] * self.nrow + idx[0] ]
    }
}


impl <'a, 'b>std::ops::Add<&'b Mat> for &'a Mat {
    type Output = Mat;
    fn add(self, inp: &'b Mat) -> Mat {
        let mut temp = Mat::new(self.nrow,self.ncol);
        temp.fill(0.);
        for i in 0..self.nrow {
            for j in 0..self.ncol {
                temp.vec[i + self.nrow*j] = self.vec[i + self.nrow*j] + inp.vec[i + self.nrow*j];
            }
        }
        return temp;
    }
}





