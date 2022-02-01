
# Examples

```rust

use tch::{self, TensorIndexer};
use tch::{Device, IndexOp, Kind, Tensor};

/// create a tensor of input batch, input data will be copied
/// Aware that
///     1. oversize inputs will be truncated
///     2. short inputs will be pad with 0
unsafe fn input_batch_to_tensor(batch: &[&[u8]], in_dim: usize) -> Tensor {
    let ts = tch::Tensor::zeros(
        &[batch.len() as i64, in_dim as i64],
        (Kind::Uint8, Device::Cpu),
    );
    (0..batch.len() as i64).zip(batch).for_each(|(row, i)| {
        let bytes = *i;
        let len = in_dim.min(bytes.len()) as i64;
        let ts_row = Tensor::of_blob(
            bytes.as_ptr(),
            &[len], // truncates the input to in_dim
            &[1],
            Kind::Uint8,
            Device::Cpu,
        );
        ts.i((row, ..len)).copy_(&ts_row);
    });
    ts
}


pub fn standardize(ts: &Tensor) -> Tensor {
    match ts.dim() {
        1 => {
            let mean = ts.mean(Kind::Float);
            let mut ct = ts - mean;
            let norm = ct.norm();
            ct /= norm;
            ct
        }
        2 => {
            let mean = ts.mean_dim(&[1], true, Kind::Float);
            let mut ct = ts - mean;
            let std = ct.std_dim(&[1], false, true);
            ct /= std;
            ct
        }
        _ => panic!("only support vectors and matrices"),
    }
}

fn print_ts(ts: &Tensor) {
    println!("{}", ts.to_string(100).unwrap());
}

fn test_standardize() {
    println!("test_standardize");
    let v: Vec<_> = (0..10).map(|i| i as f32).collect();
    let ts = Tensor::of_slice(&v);
    println!("{}", standardize(&ts).to_string(100).unwrap());
    let ts2 = ts.view((2, 5));
    println!("ts: {}", ts2.to_string(100).unwrap());
    let (std, mean) = ts2.std_mean_dim(&[1], false, true);
    println!("std: {}, mean {}", std.to_string(100).unwrap(), mean.to_string(100).unwrap());
    let nm = (ts2 - mean) / std;
    println!("normed: {}", nm.to_string(100).unwrap());
}

fn test_sum_axis() {
    println!("test_sum_axis");
    let v: Vec<_> = (0..10).map(|i| i as f32).collect();
    let ts = Tensor::of_slice(&v).view((2, 5));
    let sum = ts.sum_dim_intlist(&[0], false, Kind::Float);
    println!("sum along axis 0");
    print_ts(&sum);
}

fn main() {
    let in_dim = 10;
    let batch: Vec<_> = (0..in_dim).map(|i| vec![i as u8; i]).collect();
    let slices: Vec<_> = batch.iter().map(|x| &x[..]).collect();
    let ts = unsafe { input_batch_to_tensor(&slices, in_dim) };
    println!("{:?}", ts);
    println!("{}", ts.to_string(100).unwrap());

    println!("");
    println!("tensor indexing");
    let indexer = Tensor::of_slice(&[5, 2]);
    // select column
    let sub_mat = ts.i((.., TensorIndexer::IndexSelect(indexer)));
    println!("{}", sub_mat.to_string(100).unwrap());
    println!("stride {:?}", sub_mat.stride());

    println!("");
    println!("Shared memory of vector and tensor");
    let mut vector: Vec<i64> = vec![1; 10];
    let ts = unsafe {
        Tensor::of_blob(
            vector.as_ptr() as *const u8,
            &[10],
            &[1],
            Kind::Int64,
            Device::Cpu,
        )
    };
    println!("vec: \n   {:?}", vector);
    println!("tensor: \n{}", ts.to_string(100).unwrap());
    vector[5] = 3;
    println!(
        "tensor, ts[5] should be 3: \n{}",
        ts.to_string(100).unwrap()
    );

    println!("");
    println!("Standardize tensor");
    let ts2 = Tensor::of_slice(&[1.0; 20]).view((5, 4));
    let ts1 = ts2.i((.., 2));
    println!(
        "ts2.mean(): {}, \n ts1.mean(): {}, \n ts1.norm(): {}",
        ts2.mean(Kind::Float).to_string(100).unwrap(),
        ts1.mean(Kind::Float).to_string(100).unwrap(),
        ts1.norm().to_string(100).unwrap()
    );

    test_standardize();

    test_sum_axis();

    // any dim
    println!("");
    println!("tensor.any_dim");

    let v: Vec<_> = (0..10).map(|i| i as u8).collect();
    let ts = Tensor::of_slice(&v).view((2, 5));
    let ts_any = ts.any_dim(0, false);
    print_ts(&ts_any);
    let ts_any_vec: Vec<bool> = ts_any.into();
    println!("{:?}", ts_any_vec);

    println!("\ncan byte tensor add a float tensor?");
    let u8_ts = Tensor::of_slice(&[1 as u8; 10]);
    let f_ts = Tensor::of_slice(&[-1.0 as f32; 10]);
    let rst = u8_ts - f_ts;
    print_ts(&rst);
    // print_ts(&rst.pad);
}


```