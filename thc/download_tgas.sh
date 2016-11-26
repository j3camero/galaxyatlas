for i in $(seq 0 15); do
    data_source="http://cdn.gea.esac.esa.int/Gaia/tgas_source/csv/";
    base_filename="TgasSource_000-000-$(printf %03d $i).csv.gz";
    full_filename="$data_source$base_filename";
    echo "Fetching $i of 16: $base_filename";
    wget $full_filename;
    gunzip $base_filename;
done
