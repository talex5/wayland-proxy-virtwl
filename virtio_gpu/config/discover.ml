module C = Configurator.V1

let () =
  C.main ~name:"checkdrm" (fun c ->
      let conf =
        match C.Pkg_config.get c with
        | None -> failwith "pkg-config is not installed"
        | Some pc ->
          match (C.Pkg_config.query pc ~package:"libdrm") with
          | None -> failwith "libdrm is not installed (according to pkg-config)"
          | Some deps -> deps
      in
      C.Flags.write_sexp "c_flags.sexp"         conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs
    )
