/*
Matrix structure. Stored in vector. Column wise. 
*/

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
                print!{"{:?}", self.vec[i + self.nrow*j]};
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
}

impl std::ops::Index<[usize; 2]> for Mat{
    type Output = f64;
    fn index<'a>(&'a self, idx: [usize; 2] ) ->&'a f64{
        &self.vec[idx[1] * self.nrow + idx[0] ]
    }
}


impl std::ops::IndexMut<[usize; 2]> for Mat{
    fn index_mut<'a>(&'a mut self, idx: [usize; 2] ) ->&'a mut f64{
        &mut self.vec[idx[1] * self.nrow + idx[0] ]
    }
}



