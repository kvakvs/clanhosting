class Forum
  # Список форумов для клана и его альянсов
  def self.list(clan_id)
    rpc = Rails.application.get_rpc
    f_index = rpc.call.ch_forum_api.read_index(clan_id) || []

    # TODO: альянсы тоже! Не забыть альянсы получать парами клан_id+форум

    f_index.map! { |forum_id|
      value = rpc.call.ch_forum_api.read_one(clan_id, forum_id)
      value['id'] = forum_id
      value
    }
  end
end
