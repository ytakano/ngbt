-record(torrent,
        {info,
         announce,
         announce_list = [],  % optional
         creation_date,       % optional
         comment,             % optional
         created_by,          % optional
         encoding             % optional
        }).

-record(torrent_info,
        {piece_length,
         pieces,
         private = 0,  % optional
         name,
         length,       % single file mode
         md5sum,       % single file mode, optional
         files         % multiple file mode
        }).

-record(torrent_file,
        {length,
         path,
         md5sum  % optional
        }).
