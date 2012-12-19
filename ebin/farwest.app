{application,farwest,
             [{description,"REST-based web development environment."},
              {vsn,"0.1.0"},
              {modules,[farwest,farwest_app,farwest_base_dtl,farwest_sup,
                        fw_auth,fw_config,fw_page_handler,fw_ssl_auth_method,
                        fw_templates_collection_dtl,fw_templates_editor_dtl,
                        fw_templates_handler,fw_templates_server,fw_test,
                        fw_userdata_collection_dtl,fw_userdata_editor_dtl,
                        fw_userdata_handler,fw_userdata_server,
                        fw_userfiles_collection_dtl,fw_userfiles_handler,
                        fw_userfiles_server,fw_versioned_buckets]},
              {registered,[farwest_sup]},
              {applications,[kernel,stdlib,crypto,public_key,ssl,ranch,
                             cowboy]},
              {mod,{farwest_app,[]}},
              {env,[]}]}.