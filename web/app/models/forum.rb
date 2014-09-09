class Forum
  # Поля:
  # string id (задаётся при чтении прямо здесь, не хранится в БД)
  # string title utf8
  # string desc utf8

  # Список форумов для клана и его альянсов
  def self.list(clan_id)
    rpc = Rails.application.get_rpc
    f_index = rpc.call.ch_forum_api.read_index(clan_id) || []

    # TODO: альянсы тоже! Не забыть альянсы получать парами клан_id+форум

    f_index.map! { |forum_id|
      value = rpc.call.ch_forum_api.read_one(clan_id, forum_id)
      value['id'] = forum_id
      value['title'] = value['title'].force_encoding('utf-8')
      value['desc'] = value['desc'].force_encoding('utf-8')
      value
    }
  end

  def self.create(clan_id, fields)
    rpc = Rails.application.get_rpc
    rpc.call.ch_forum_api.add_forum(clan_id, fields)
  end

  def self.delete(clan_id, forum_id)
    rpc = Rails.application.get_rpc
    rpc.call.ch_forum_api.delete_forum(clan_id, forum_id)
  end
end
