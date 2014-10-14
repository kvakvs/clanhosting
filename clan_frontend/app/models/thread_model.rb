require 'ch_lib'

class ThreadModel
  # Поля:
  # string id (задаётся при чтении прямо здесь, не хранится в БД)
  # string title utf8
  # int created_by

  # Список тредов для форума
  def self.list(clan_id, forum_id)
    rpc = Rails.application.get_rpc
    f_index = rpc.call.ch_thread_api.read_index(clan_id, forum_id) || []
    f_index.sort!

    # TODO: Paginate

    f_index.map! { |thread_id|
      read_one(clan_id, forum_id, thread_id)
    }
  end

  def self.read_one(clan_id, forum_id, thread_id)
    rpc = Rails.application.get_rpc
    value = rpc.call.ch_thread_api.read_one(clan_id, forum_id, thread_id)
    return nil if value.nil?
    value['id'] = thread_id
    value['created_at_d'] = DateTime.iso8601(value['created_at'])
    value['updated_at_d'] = DateTime.iso8601(value['updated_at'])
    ChLib.from_rpc(value)
    # value['title'] = value['title'].force_encoding('utf-8')
  end

  def self.create(clan_id, forum_id, fields)
    rpc = Rails.application.get_rpc
    rpc.call.ch_thread_api.add_thread(clan_id, forum_id, fields)
  end

  def self.delete(clan_id, forum_id, thread_id)
    rpc = Rails.application.get_rpc
    rpc.call.ch_thread_api.delete_thread(clan_id, forum_id, thread_id)
  end
end
