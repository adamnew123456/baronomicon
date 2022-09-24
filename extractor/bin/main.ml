(* Utility functions *)

(** Gets a list of all files within a directory that have an extension,
    recursively. All paths returned include basedir as a prefix. *)
let readdir_recursive (suffix: string) (basedir: string) =
  let rec categorize_entries (queue, output) path =
    if Sys.is_directory path then
      (path :: queue, output)
    else if String.ends_with ~suffix:suffix path then
      (queue, path :: output)
    else
      (queue, output)
  and traverse queue output =
    match queue with
    | dir :: queue_tail ->
      let (new_queue, new_output) =
        Sys.readdir dir
        |> Array.map (Filename.concat dir)
        |> Array.fold_left categorize_entries (queue_tail, output) in
      traverse new_queue new_output
    | _ -> output in
  traverse [basedir] []

(*

(** Writes out an XML parser event. *)
let dump_xml_event (stream: Xmlm.input) =
  let (line, col) = Xmlm.pos stream in
  let event = Xmlm.input stream in
  begin
    print_endline "-------------------------------" ;
    (match event with
    | `El_start ((ns, tag), attrs) ->
      let attr_list =
        attrs
        |> List.map (fun ((ns, name), value) -> Printf.sprintf "'%s':'%s'='%s'" ns name value)
        |> String.concat " " in
      Printf.printf "%d:%d El_start '%s':'%s' %s\n" line col ns tag attr_list

    | `El_end -> Printf.printf "%d:%d El_end\n" line col
    | `Data data -> Printf.printf "%d:%d Data '%s'\n" line col data
    | `Dtd dtd ->
      begin
        match dtd with
        | Some dtd -> Printf.printf "%d:%d Dtd '%s'\n" line col dtd
        | None -> Printf.printf "%d:%d Dtd NULL\n" line col
      end) ;
    Printexc.get_callstack 5
    |> Printexc.print_raw_backtrace Out_channel.stdout ;
    event
  end
*)

(** Indicates that an XML subtree should be ignored. Should be started after the
    to-be-ignored element has been read, and continues reading until that element
    is closed. Calls the continuation when done. *)
let ignore_subtree (stream: Xmlm.input) (k: Xmlm.input -> 'a) =
  let rec ignore depth =
    if Xmlm.eoi stream then
      k stream
    else
      match Xmlm.input stream with
      | `El_start _ -> ignore (depth + 1)
      | `El_end ->
        if depth = 1 then
          k stream
        else
          ignore (depth - 1)
      | _ -> ignore depth in
  ignore 1

(** Fetches a value from an association list, used for XMl attributes. *)
let rec find_attr (alist: ((string * string) * 'a) list) (attr: string) =
  match alist with
  | ((_, akey), aval) :: rest ->
    if String.lowercase_ascii akey = String.lowercase_ascii attr then
      Some aval
    else
      find_attr rest attr

  | [] -> None

(* Data model *)

type item_reference =
  | ItemId of string
  | ItemTag of string

(** An item used to fabricate another item, or that comes from deconstructing
    that item. *)
type recipe_part = {
  item: item_reference;
  amount: int;
  min_condition: float option;
  max_condition: float option;
}

(** A recipe consists of multiple components, all of which are required. *)
type recipe = recipe_part list

(** An individual item, including the recipes for fabricating or destructing it (if supported)*)
type item_decl = {
  id: string;
  tags: string list;
  fabricate: recipe list;
  deconstruct: recipe list;
}

(* Serialization *)

let recipe_part_to_json (part: recipe_part) : Yojson.t =
  let item_ref =
    match part.item with
    | ItemId id -> `Assoc [("id", `String id)]
    | ItemTag tag -> `Assoc [("tag", `String tag)] in
  `Assoc [("item", item_ref);
          ("amount", `Int part.amount);
          ("min_condition",
           match part.min_condition with
           | Some cond -> `Float cond
           | None -> `Null);
          ("max_condition",
           match part.max_condition with
           | Some cond -> `Float cond
           | None -> `Null)]

let recipe_to_json (recipe: recipe) : Yojson.t =
  `List (List.map recipe_part_to_json recipe)

let item_decl_to_json (item_labels: (string, string) Hashtbl.t) (item_descriptions: (string, string) Hashtbl.t) (item: item_decl) : Yojson.t =
  let label =
    Hashtbl.find_opt item_labels item.id
    |> Option.map (fun label -> `String label)
    |> Option.value ~default:`Null in
  let description =
    Hashtbl.find_opt item_descriptions item.id
    |> Option.map (fun name -> `String name)
    |> Option.value ~default:`Null in
  `Assoc [("id", `String item.id);
          ("label", label);
          ("description", description);
          ("tags", `List (List.map (fun tag -> `String tag) item.tags));
          ("fabricate", `List (List.map recipe_to_json item.fabricate));
          ("deconstruct", `List (List.map recipe_to_json item.deconstruct))]

(* Resource file parsing *)

(** Parses an Item file from XML into a list of item declarations. *)
let parse_item_file (path: string) =
  let rec build_recipe_part attrs =
    let item =
      match (find_attr attrs "identifier", find_attr attrs "tag") with
      | (Some id, _) -> ItemId id
      | (_, Some tag) -> ItemTag tag
      | _ -> raise Not_found in
    let amount =
      match find_attr attrs "amount" with
      | Some amount_str -> int_of_string amount_str
      | None -> 1 in
    let use_condition = find_attr attrs "usecondition" = Some "false" in
    if use_condition then
      {item; amount; min_condition=None; max_condition=None}
    else
      let min_condition = find_attr attrs "mincondition" |> Option.map float_of_string in
      let max_condition = find_attr attrs "maxcondition" |> Option.map float_of_string in
      {item; amount; min_condition; max_condition}

  and parse_fabricate items item_builder fab_list stream =
    match Xmlm.input stream with
    | `El_start ((_, "RequiredItem"), attrs) ->
      let updated_fab_list = build_recipe_part attrs :: fab_list in
      ignore_subtree stream (parse_fabricate items item_builder updated_fab_list) (* Throw away the end element *)

    | `El_start _ -> ignore_subtree stream (parse_fabricate items item_builder fab_list)
    | `El_end ->
      let updated_item = {
        item_builder with
        fabricate = fab_list :: item_builder.fabricate} in
      parse_item items updated_item stream

    | _ -> parse_fabricate items item_builder fab_list stream

  and parse_deconstruct items item_builder decon_list stream =
    match Xmlm.input stream with
    | `El_start ((_, "Item"), attrs) ->
      let updated_decon_list = build_recipe_part attrs :: decon_list in
      ignore_subtree stream (parse_deconstruct items item_builder updated_decon_list)

    | `El_start _ ->
      ignore_subtree stream (parse_deconstruct items item_builder decon_list)

    | `El_end ->
      let updated_item = {
        item_builder with
        deconstruct = decon_list :: item_builder.deconstruct} in
      parse_item items updated_item stream

    | _ -> parse_deconstruct items item_builder decon_list stream

  and parse_item items item_builder stream =
    match Xmlm.input stream with
    | `El_start ((_, "Fabricate"), _) -> parse_fabricate items item_builder [] stream
    | `El_start ((_, "Deconstruct"), _) -> parse_deconstruct items item_builder [] stream
    | `El_start (_, _) -> ignore_subtree stream (parse_item items item_builder)
    | `El_end -> parse_items (item_builder :: items) stream
    | _ -> parse_item items item_builder stream

  and parse_items items stream =
    match Xmlm.input stream with
    | `El_start ((_, _), attrs) ->
      begin
        match find_attr attrs "identifier" with
        | Some item_id ->
          let tags =
            find_attr attrs "tags"
            |> Option.map (String.split_on_char ',')
            |> Option.map (List.map String.trim)
            |> Option.value ~default:[] in
          let item_builder = {
            id=item_id;
            tags;
            fabricate=[];
            deconstruct=[]} in
          parse_item items item_builder stream

        (* Legacy Items don't have an identifier *)
        | None -> ignore_subtree stream (parse_items items)
      end
    | `El_end -> items
    | _ -> parse_items items stream in

  In_channel.with_open_bin path (fun input ->
      let stream = Xmlm.make_input (`Channel input) in
      begin
        (* Consume the DTD, these files don't have one but the parser always outputs one *)
        Xmlm.input stream |> ignore;

        match Xmlm.input stream with
        | `El_start (("", "Items"), _) -> parse_items [] stream
        | _ -> []
      end)

let entity_name_prefix = "entityname."
let entity_name_prefix_len = String.length entity_name_prefix

let entity_desc_prefix = "entitydescription."
let entity_desc_prefix_len = String.length entity_desc_prefix

(** Parses a Text file from XML into a map from IDs to labels and descriptions. *)
let parse_label_file (item_labels: (string, string) Hashtbl.t) (item_descriptions: (string, string) Hashtbl.t) (path: string) =
  let rec parse_content id hashtbl stream =
    match Xmlm.input stream with
    | `Data content ->
      begin
        Hashtbl.replace hashtbl id content ;
        ignore_subtree stream parse_texts
      end
    | `El_end -> parse_texts stream
    | _ -> ignore_subtree stream parse_texts

  and parse_texts stream =
    if Xmlm.eoi stream then
      ()
    else
      match Xmlm.input stream with
      | `El_start ((_, name), _) ->
        begin
          let (line, col) = Xmlm.pos stream in
            Printf.fprintf Out_channel.stderr "%s:%d:%d %s\n" path line col name ;
          if String.starts_with ~prefix:entity_name_prefix name then
            let id = String.sub name entity_name_prefix_len (String.length name - entity_name_prefix_len) in
            parse_content id item_labels stream
          else if String.starts_with ~prefix:entity_desc_prefix name then
            let id = String.sub name entity_desc_prefix_len (String.length name - entity_desc_prefix_len) in
            parse_content id item_descriptions stream
          else
            ignore_subtree stream parse_texts
        end
      | _ -> parse_texts stream in

  In_channel.with_open_bin path (fun input ->
      let stream = Xmlm.make_input (`Channel input) in
      begin
        (* Consume the DTD, these files don't have one but the parser always outputs one *)
        Xmlm.input stream |> ignore;

        match Xmlm.input stream with
        | `El_start (("", "infotexts"), attrs) ->
          if find_attr attrs "language" = Some "English" then
            parse_texts stream
          else
            ()
        | _ -> ()
      end)

(* Cleanup and filtering *)

module ItemSet = Set.Make(
    struct
      type t = string
      let compare = String.compare
    end)

(** Builds a map of tags to the items the tag is applied to. *)
let find_item_tags (items: item_decl list) =
  let rec tag_map = Hashtbl.create (List.length items)
  and add_item_tag id tag =
    let old_list =
      Hashtbl.find_opt tag_map tag
      |> Option.value ~default:[] in
    Hashtbl.replace tag_map tag (id :: old_list)

  and add_item_tags item =
    List.iter (add_item_tag item.id) item.tags in

  begin
    List.iter add_item_tags items ;
    tag_map
  end

(** Removes all items that lack recipes and are not referenced by another recipe. *)
let prune_items (items: item_decl list) (by_tag: (string, string list) Hashtbl.t) =
  let rec scan_recipe_part keep part =
    match part.item with
    | ItemId id -> ItemSet.add id keep
    | ItemTag tag ->
      begin
        match Hashtbl.find_opt by_tag tag with
        | None -> keep
        | Some item_ids -> ItemSet.add_seq (List.to_seq item_ids) keep
      end

  and scan_recipe keep recipe =
    List.fold_left scan_recipe_part keep recipe

  and scan_item keep item =
    let keep = List.fold_left scan_recipe keep item.fabricate in
    let keep = List.fold_left scan_recipe keep item.deconstruct in
    if List.length item.fabricate > 0 || List.length item.deconstruct > 0 then
      ItemSet.add item.id keep
    else
      keep in

  let keep_items = List.fold_left scan_item ItemSet.empty items in
  List.filter (fun item -> ItemSet.mem item.id keep_items) items

(** Combines multiple items in a recipe that are all the same *)
let compact_recipe (recipe: recipe) =
  let rec is_part_same a b =
    a.item = b.item
    && a.min_condition = b.min_condition
    && a.max_condition = b.max_condition

  and combine_same_parts parts =
    let first = List.hd parts in
    let rest = List.tl parts in
    {first with amount=List.fold_left (fun amount item -> amount + item.amount) first.amount rest}

  and compact_part remaining result =
    match remaining with
    | [] -> result
    | hd :: rest ->
      let (matching_hd, others) = List.partition (is_part_same hd) rest in
      let new_hd = combine_same_parts (hd :: matching_hd) in
      compact_part others (new_hd :: result) in

  compact_part recipe []

(** Compacts all the recipes for an item *)
let compact_item_recipes (item: item_decl) =
  {item with fabricate=List.map compact_recipe item.fabricate;
             deconstruct=List.map compact_recipe item.deconstruct}

let () =
  begin
    Printexc.record_backtrace true ;
    let xml_files =
      Array.to_list Sys.argv
      |> List.tl
      |> List.concat_map (readdir_recursive ".xml") in
    let items =
      List.concat_map parse_item_file xml_files
      |> List.map compact_item_recipes in
    let item_labels = Hashtbl.create 256 in
    let item_descriptions = Hashtbl.create 256 in
    let by_tag = find_item_tags items in
    begin
      List.iter (parse_label_file item_labels item_descriptions) xml_files ;
      let items_json =
        prune_items items by_tag
        |> List.map (item_decl_to_json item_labels item_descriptions) in
      begin
        Yojson.to_channel Out_channel.stdout (`List items_json) ;
        Out_channel.flush Out_channel.stdout
      end
    end
  end
