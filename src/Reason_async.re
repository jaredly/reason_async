type either('a, 'b) =
  | Left('a)
  | Right('b);

module type MonadThing = {
  type t('a);
  let return: 'a => t('a);
  let map: (t('a), ~f: 'a => 'b) => t('b);
  let bind: (t('a), ~f: 'a => t('b)) => t('b);
  let consume: (t('a), ~f: 'a => unit) => unit;
  let join2: (t('a), t('b)) => t(('a, 'b));
  /*let mapl: list (t 'a) => f::('a => 'b) => t (list 'b);*/
  /*let bindl: list (t 'a) => f::('a => t 'b) => t (list 'b);*/
  /* let first: t 'a => t 'b => t (either 'a 'b); */
};

module Continuations /*: MonadThing with type t 'a = ('a => unit) => unit */ = {
  type t('a) = ('a => unit) => unit;
  let return = (x, fin) => fin(x);
  let map = (work, ~f as use, fin) => work((result) => fin(use(result)));
  let bind = (work, ~f as use, fin) => work((result) => (use(result))(fin));
  let consume = (work, ~f as use) => work(use);
  type side('a, 'b) =
    | One('a)
    | Two('b)
    | Neither
    | Done;
  let join2 = (one, two, fin) => {
    let side = ref(Neither);
    one(
      (one) =>
        switch side^ {
        | Neither => side := One(one)
        | Two(two) =>
          side := Done;
          fin((one, two))
        /* not allowed to call multiple times */
        | One(_)
        | Done => ()
        }
    );
    two(
      (two) =>
        switch side^ {
        | Neither => side := Two(two)
        | One(one) =>
          side := Done;
          fin((one, two))
        /* not allowed to call multiple times */
        | Two(_)
        | Done => ()
        }
    )
  };
  let first = (one, two, fin) => {
    let finished = ref(false);
    one(
      (one) =>
        if (! finished^) {
          finished := true;
          fin(Left(one))
        }
    );
    two(
      (two) =>
        if (! finished^) {
          finished := true;
          fin(Right(two))
        }
    )
  };
};

/*: MonadThing with type t ('a, 'b) = (result 'a 'b => unit) => unit */
module NodeContinuations = {
  type t('a, 'b) = (result('a, 'b) => unit) => unit;
  let return = (x, fin) => fin(Ok(x));
  let map = (work, ~f as use, fin) => work((result) => fin(Ok(use(result))));
  let bind = (work, ~f as use, fin) => work((result) => (use(result))(fin));
  let consume = (work, ~f as use) => work(use);
  type side('a, 'b) =
    | One('a)
    | Two('b)
    | Neither
    | Done;
  let join2 = (one, two, fin) => {
    let side = ref(Neither);
    one(
      (one) =>
        switch side^ {
        | Neither =>
          switch one {
          | Ok(one) => side := One(one)
          | Error(err) =>
            side := Done;
            fin(Error(err))
          }
        | Two(two) =>
          switch one {
          | Ok(one) =>
            side := Done;
            fin((one, two))
          | Error(err) => fin(Error(err))
          }
        /* not allowed to call multiple times */
        | One(_)
        | Done => ()
        }
    );
    two(
      (two) =>
        switch side^ {
        | Neither =>
          switch two {
          | Ok(two) => side := Two(two)
          | Error(err) =>
            side := Done;
            fin(Error(err))
          }
        | One(one) =>
          switch two {
          | Ok(two) =>
            side := Done;
            fin((one, two))
          | Error(err) => fin(Error(err))
          }
        /* not allowed to call multiple times */
        | One(_)
        | Done => ()
        }
    )
  };
};

module Option /*: MonadThing with type t 'a = option 'a */ = {
  type t('a) = option('a);
  let return = (x) => Some(x);
  let map = (value, ~f as use) =>
    switch value {
    | Some(x) => Some(use(x))
    | None => None
    };
  let bind = (value, ~f as use) =>
    switch value {
    | Some(x) => use(x)
    | None => None
    };
  let consume = (value, ~f as use) =>
    switch value {
    | Some(x) => use(x)
    | None => ()
    };
  let join2 = (one, two) =>
    switch one {
    | None => None
    | Some(one) =>
      switch two {
      | None => None
      | Some(two) => Some((one, two))
      }
    };
  let first = (one, two) =>
    switch one {
    | Some(one) => Some(Left(one))
    | None =>
      switch two {
      | Some(two) => Some(Right(two))
      | None => None
      }
    };
};

module Results = {
  open Js.Result;
  let return = (x) => Ok(x);
  let map /*: t 'a 'b => f::('a => 'c) => t 'c 'b*/ = (value, ~f as use) =>
    switch value {
    | Ok(x) => Ok(use(x))
    | Error(e) => Error(e)
    };
  let bind: (t('a, 'b), ~f: 'a => t('c, 'b)) => t('c, 'b) =
    (value, ~f as use) =>
      switch value {
      | Ok(x) => use(x)
      | Error(e) => Error(e)
      };
  let consume: (t('a, 'b), ~f: 'a => unit) => unit =
    (value, ~f as use) =>
      switch value {
      | Ok(x) => use(x)
      | Error(_) => assert false /* TODO maybe have a different fail pattern? */
      };
  let join2 = (one, two) =>
    switch one {
    | Error(e) => Error(e)
    | Ok(v1) =>
      switch two {
      | Error(e) => Error(e)
      | Ok(v2) => Ok((v1, v2))
      }
    };
  let first = (one, two) =>
    switch one {
    | Ok(x) => Ok(Left(x))
    | Error(e) =>
      switch two {
      | Ok(x) => Ok(Right(x))
      | Error(e) => Error(e) /* maybe have the error include both? */
      }
    };
  let unwrap = (value, map, other_return) =>
    switch value {
    | Ok(x) => map(x, ~f=return)
    | Error(err) => other_return(Error(err))
    };
};
