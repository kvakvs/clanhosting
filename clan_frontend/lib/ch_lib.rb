class ChLib
  # Переводит строковые поля пришедшие из RPC в UTF8
  def self.from_rpc(dict)
    dict.each_key { |k|
      if dict[k].is_a? String
        dict[k] = dict[k].force_encoding('utf-8')
      end
    }
    dict
  end

  def self.str_from_rpc(s)
    s.force_encoding('utf-8')
  end
end
