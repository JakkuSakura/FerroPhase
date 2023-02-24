pub struct SourceProcess {
    pub name: &'static str,
    pub val: i64,
}

impl SourceProcess {
    pub fn spawn(name: &'static str, val: i64) -> SourceProcess {
        SourceProcess { name, val }
    }
}

impl Actor<()> for SourceProcess {
    type Stdout = i64;

    fn process(&self, _item: ()) -> Result<Self::Stdout, Stderr> {
        println!("This is {}, output is {}", self.name, self.val);
        Ok(self.val)
    }
}

pub struct AddProcess {
    pub name: &'static str,
}

impl AddProcess {
    pub fn spawn(name: &'static str) -> AddProcess {
        AddProcess { name }
    }
    pub fn add_inner(&self, item: i64, v: i64) -> i64 {
        let output = item + v;
        println!(
            "This is {}, item is {}, output is {}",
            self.name, item, output
        );
        output
    }
    pub fn add(&self, v: i64) -> impl Actor<i64, Stdout = i64> + '_ {
        ActorFn::new(move |i| Ok(self.add_inner(i, v)))
    }
}

impl Actor<i64> for AddProcess {
    type Stdout = i64;
    fn process(&self, item: i64) -> Result<Self::Stdout, Stderr> {
        Ok(self.add_inner(item, 1))
    }
}

pub struct SinkProcess {
    pub name: &'static str,
}

impl SinkProcess {
    pub fn spawn(name: &'static str) -> SinkProcess {
        SinkProcess { name }
    }
}

impl Actor<i64> for SinkProcess {
    type Stdout = ();

    fn process(&self, item: i64) -> Result<Self::Stdout, Stderr> {
        println!("This is {}, result is {}", self.name, item);

        Ok(())
    }
}
